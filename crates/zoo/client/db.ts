import { createDbWorker, WorkerHttpvfs } from "sql.js-httpvfs";
import { useEffect, useRef, useState } from "preact/hooks";

import zooDbUrl from "../zoo.db?url";

const workerUrl = new URL(
  "sql.js-httpvfs/dist/sqlite.worker.js",
  import.meta.url,
);
const wasmUrl = new URL("sql.js-httpvfs/dist/sql-wasm.wasm", import.meta.url);

let _worker: Promise<WorkerHttpvfs>;
export function connectToDatabase(): Promise<WorkerHttpvfs> {
  return (_worker ??= createDbWorker(
    [
      {
        from: "inline",
        config: {
          serverMode: "full",
          url: zooDbUrl,
          // Should match sqlite page size pragma
          requestChunkSize: 4 * 1024,
        },
      },
    ],
    workerUrl.toString(),
    wasmUrl.toString(),
  ));
}

// null = loading
export function useQuery<T>(
  sql: string,
  params?: unknown[] | object,
): null | T[] {
  const [data, setData] = useState<null | T[]>(null);
  const id = useRef(0);

  useEffect(() => {
    (async () => {
      const runId = ++id.current;
      const { db } = await connectToDatabase();
      const data = await db.query(sql, params);
      if (runId === id.current) {
        setData(data as T[]);
      }
    })();
  }, [sql, JSON.stringify(params)]);

  return data;
}

// null = loading
// undefined = no result
export function useQueryOne<T>(
  sql: string,
  params?: unknown[] | object,
): null | undefined | T {
  const rows = useQuery<T>(sql, params);
  return rows === null ? null : rows[0];
}
