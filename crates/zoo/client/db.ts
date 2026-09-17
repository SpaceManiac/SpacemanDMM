import { createDbWorker, WorkerHttpvfs } from "sql.js-httpvfs";
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
