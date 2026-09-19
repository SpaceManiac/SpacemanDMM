import { ComponentChildren, render } from "preact";
import { useSyncExternalStore } from "preact/compat";
import { AnsiHtml } from "fancy-ansi/react";
import { useQuery, useQueryOne } from "./db";
import "./zoo.css";

export interface ZooProps {}

export function renderZoo(element: HTMLElement, props: ZooProps = {}) {
  render(<Zoo {...props} />, element);
}

export function Zoo(props: ZooProps) {
  const hash = useHash();
  const params = Object.fromEntries(
    new URLSearchParams(hash.substring(1)).entries(),
  );

  return (
    <>
      <h1>SpacemanDMM zoo</h1>

      <fieldset>
        <a href="#">Home</a>
      </fieldset>

      {params.codebase &&
      params.git_rev !== undefined &&
      params.spaceman_rev ? (
        // Run detail
        <SingleRunDetail
          codebase={params.codebase}
          git_rev={params.git_rev}
          spaceman_rev={params.spaceman_rev}
        />
      ) : params.codebase && params.git_rev !== undefined ? (
        // Revision detail, many SpacemanDMM versions
        <RevisionDetail codebase={params.codebase} git_rev={params.git_rev} />
      ) : (
        // Many codebases, many SpacemanDMM versions
        <CodebaseBySpaceman codebase={params.codebase} />
      )}
    </>
  );
}

function SingleRunDetail(props: {
  codebase: string;
  git_rev: string;
  spaceman_rev: string;
}) {
  const tags = useSpacemanTags();
  const reverseTags = Object.fromEntries(
    tags.map((tag) => [tag.spaceman_rev, tag.name]),
  );

  const data = useQueryOne<DbCodebase & DbCommit & DbTree & DbTreeAnalysis>(
    `
      SELECT *
      FROM codebase
      LEFT JOIN codebase_commit
        ON codebase.codebase = codebase_commit.codebase
      LEFT JOIN "commit"
        ON codebase_commit."commit" = "commit"."commit"
      LEFT JOIN tree
        ON "commit".tree = tree.tree
      LEFT JOIN tree_analysis
        ON "commit".tree = tree_analysis.tree
      WHERE codebase.codebase = :codebase
        AND "commit"."commit" = :git_rev
        AND tree_analysis.spaceman_rev = :spaceman_rev
    `,
    {
      ":codebase": props.codebase,
      ":git_rev": props.git_rev,
      ":spaceman_rev": props.spaceman_rev,
    },
  );

  const analysis: Panic | Analysis = data ? JSON.parse(data.analysis) : null;

  return (
    <>
      <h3>
        Run detail: {props.codebase}
        {data ? (
          <>
            {" "}
            <RevisionLink row={data}>{data.date.substring(0, 10)}</RevisionLink>
            {" on SpacemanDMM "}
            {reverseTags[data.spaceman_rev] || data.spaceman_rev}
          </>
        ) : null}
      </h3>
      {data === null ? (
        <>Loading...</>
      ) : data === undefined ? (
        <>No results.</>
      ) : (
        <>
          {data.config ? (
            <fieldset>
              <legend>SpacemanDMM.toml</legend>
              <pre>{data.config}</pre>
            </fieldset>
          ) : null}
          <fieldset>
            <legend>Error detail</legend>
            <pre>
              <AnsiHtml
                text={
                  "panic" in analysis
                    ? analysis.panic
                    : analysis.errors.trimEnd()
                }
              />
            </pre>
          </fieldset>
        </>
      )}
    </>
  );
}

function RevisionDetail(props: { codebase: string; git_rev: string }) {
  const data = useQueryOne<DbCodebase & DbCommit & DbCodebaseCommit>(
    `
      SELECT *
      FROM analysis
      LEFT JOIN config
      ON analysis.codebase = config.codebase
        AND analysis.git_rev = config.git_rev
      WHERE analysis.codebase = :codebase
        AND analysis.git_rev = :git_rev
    `,
    {
      ":codebase": props.codebase,
      ":git_rev": props.git_rev,
    },
  );

  return (
    <>
      <h3>
        Revision detail: {props.codebase}
        {data ? (
          <>
            {" "}
            <RevisionLink row={data}>{data.date.substring(0, 10)}</RevisionLink>
          </>
        ) : null}
      </h3>
      {data === null ? (
        <>Loading...</>
      ) : data === undefined ? (
        <>No results.</>
      ) : (
        <>TODO</>
      )}
    </>
  );
}

function CodebaseBySpaceman(props: {
  codebase?: string;
  date_max?: string;
  date_min?: string;
}) {
  const tags = useSpacemanTags();

  const data = useQuery<
    DbCodebase & DbCodebaseCommit & DbCommit & DbTree & DbTreeAnalysis
  >(
    `
    SELECT *
    FROM codebase
    INNER JOIN codebase_commit
      ON codebase.codebase = codebase_commit.codebase
    INNER JOIN "commit"
      ON codebase_commit."commit" = "commit"."commit"
    INNER JOIN tree
      ON "commit".tree = tree.tree
    INNER JOIN tree_analysis
      ON tree.tree = tree_analysis.tree
    WHERE (codebase.codebase = :codebase OR :codebase IS NULL)
    ORDER BY date DESC
  `,
    {
      ":codebase": props.codebase || null,
    },
  );

  let rows: Record<
    string,
    (DbCodebase & DbCodebaseCommit & DbCommit & DbTree & DbTreeAnalysis)[]
  > = {};
  let rowOrder: string[] = [];
  if (props.codebase) {
    // row = commit, column = SpacemanDMM version
    for (const row of data || []) {
      (rows[row.commit] ??= []).push(row);
    }
    rowOrder = Object.keys(rows);
  } else if (data) {
    // row = codebase, column = SpacemanDMM version, each cell takes MAX(date) within that cell
    for (const row of data || []) {
      const columns = (rows[row.codebase] ??= []);
      const existing = columns.findIndex(
        (cell) => cell.spaceman_rev == row.spaceman_rev,
      );
      if (existing !== -1) {
        if (columns[existing]!.date < row.date) {
          columns[existing] = row;
        }
      } else {
        columns.push(row);
      }
    }
    rowOrder = Object.keys(rows);
  }

  const spacemanRevSet = new Set(
    rowOrder.flatMap((key) => rows[key]!).map((cell) => cell.spaceman_rev),
  );
  const columns = tags.filter((tag) => spacemanRevSet.has(tag.spaceman_rev));

  return (
    <>
      <h3>
        {props.codebase ? (
          <>Runs for {props.codebase}</>
        ) : (
          <>Latest run for each codebase</>
        )}
      </h3>
      <table>
        <thead>
          <tr>
            <th colspan={4}></th>
            {columns.map((tag) => (
              <RunHeader1 {...tag} />
            ))}
          </tr>
          <tr>
            <th rowspan={2}>Codebase</th>
            <th rowspan={2}>Date</th>
            <th rowspan={2} colspan={2}>
              Config
            </th>
            {columns.map(() => (
              <RunHeader2 />
            ))}
          </tr>
          <tr>
            {columns.map(() => (
              <RunHeader3 />
            ))}
          </tr>
        </thead>
        <tbody>
          {!data ? (
            <tr>
              <td colspan={100}>Loading...</td>
            </tr>
          ) : !rowOrder?.length ? (
            <tr>
              <td colspan={100}>No results.</td>
            </tr>
          ) : (
            rowOrder.map((key) => {
              const group = rows[key]!;
              const row = group[0]!;
              const row0 = row;

              const date = row.date?.substring(0, 10);

              return (
                <tr>
                  {/* Codebase part */}
                  <td>
                    {props.codebase ? (
                      row.codebase
                    ) : (
                      <a href={`#codebase=${row.codebase}`}>{row.codebase}</a>
                    )}
                  </td>
                  <td>
                    <RevisionLink row={row}>{date}</RevisionLink>
                  </td>
                  <td>{row.suite}</td>
                  <td class="icon">
                    {row.config && row.config.match(/dreamchecker = true/) ? (
                      <a
                        href={`#codebase=${row.codebase}&git_rev=${row.commit}`}
                        title="config: dreamchecker = true"
                      >
                        {CONFIG_DREAMCHECKER}
                      </a>
                    ) : row.config ? (
                      <a
                        href={`#codebase=${row.codebase}&git_rev=${row.commit}`}
                        title="config present"
                      >
                        {CONFIG_MISC}
                      </a>
                    ) : (
                      <a
                        href={`#codebase=${row.codebase}&git_rev=${row.commit}`}
                        title="no config"
                      >
                        {CONFIG_NONE}
                      </a>
                    )}
                  </td>
                  {/* Analysis part */}
                  {columns.map((tag) => {
                    const row = group.find(
                      (row) => row.spaceman_rev == tag.spaceman_rev,
                    );
                    return row ? (
                      <RunEntry
                        codebase={row.codebase}
                        git_rev={row.commit || ""}
                        spaceman_rev={row.spaceman_rev}
                        analysis={JSON.parse(row.analysis)}
                        configured={
                          row.spaceman_rev ===
                          tags.find((t) => t.name === row0.suite)?.spaceman_rev
                        }
                        old={row.commit !== row0.commit ? row.date : undefined}
                      />
                    ) : (
                      <td colspan={7} />
                    );
                  })}
                </tr>
              );
            })
          )}
        </tbody>
      </table>
    </>
  );
}

function RunHeader1(props: { name: string }) {
  return <th colspan={7}>{props.name}</th>;
}

function RunHeader2() {
  return (
    <>
      <th rowspan={2}></th>
      <th colspan={3}>Parse</th>
      <th colspan={3}>Check</th>
    </>
  );
}

function RunHeader3() {
  return (
    <>
      <th class="error-count icon" title="Errors">
        {ERROR}
      </th>
      <th class="error-count icon" title="Warnings">
        {WARNING}
      </th>
      <th class="error-count icon" title="Infos">
        {INFO}
      </th>
      <th class="error-count icon" title="Errors">
        {ERROR}
      </th>
      <th class="error-count icon" title="Warnings">
        {WARNING}
      </th>
      <th class="error-count icon" title="Infos">
        {INFO}
      </th>
    </>
  );
}

function RunEntry(props: {
  codebase: string;
  git_rev: string;
  spaceman_rev: string;
  analysis: Analysis | Panic;
  configured: boolean;
  old?: string;
}) {
  const { codebase, git_rev, spaceman_rev, analysis } = props;
  return (
    <>
      <td
        class={`icon ${props.configured ? "configured" : ""} ${props.old ? "old" : ""}`}
      >
        <a
          href={`#codebase=${codebase}&git_rev=${git_rev}&spaceman_rev=${spaceman_rev}`}
          title={`run details${props.configured ? "\nmatches suite used in CI" : ""}${props.old ? `\nold commit: ${props.old} UTC` : ""}`}
        >
          {LOG}
        </a>
      </td>
      {"panic" in analysis ? (
        <td class="icon left" colspan={6} title={analysis.panic}>
          {FATAL_PARSER_ERROR}
        </td>
      ) : (
        <>
          <ErrorCount amount={analysis.parse_errors.error} />
          <ErrorCount amount={analysis.parse_errors.warning} />
          <ErrorCount amount={analysis.parse_errors.info} />
          {analysis.fatal_errored ? (
            <td class="icon left" colspan={3} title="fatal parser error">
              {FATAL_PARSER_ERROR}
            </td>
          ) : (
            <>
              <ErrorCount amount={analysis.dreamchecker_errors.error} />
              <ErrorCount amount={analysis.dreamchecker_errors.warning} />
              <ErrorCount amount={analysis.dreamchecker_errors.info} />
            </>
          )}
        </>
      )}
    </>
  );
}

function ErrorCount(props: { amount: number }) {
  return <td class="error-count">{props.amount || null}</td>;
}

const ERROR = "⛔";
const WARNING = "⚠️";
const INFO = "ℹ️";

const FATAL_PARSER_ERROR = ERROR;
const CONFIG_NONE = "⚪";
const CONFIG_MISC = "⚙️";
const CONFIG_DREAMCHECKER = "🟢";

const LOG = "📄";

// const rootClass = "zoo";
// function bem(e: string, m: Record<string, unknown> = {}): string {
//   return [
//     `${rootClass}__${e}`,
//     ...Object.entries(m)
//       .filter(([k, v]) => v)
//       .map(([k, v]) => `${rootClass}__${e}--${k}`),
//   ].join(" ");
// }

function useHash(): string {
  return useSyncExternalStore(
    (flush) => {
      const listener = () => {
        flush();
      };
      window.addEventListener("hashchange", listener);
      return () => window.removeEventListener("hashchange", listener);
    },
    () => location.hash,
  );
}

function useSpacemanTags(): { name: string; spaceman_rev: string }[] {
  return (
    useQuery<DbSpacemanTag & DbSpacemanRev>(
      `
        SELECT name, spaceman_tag.spaceman_rev, date
        FROM spaceman_tag
        JOIN spaceman_rev ON spaceman_tag.spaceman_rev = spaceman_rev.spaceman_rev
        ORDER BY date DESC
      `,
    ) || []
  );
}

function RevisionLink(props: {
  row?: (DbCodebase & DbCodebaseCommit & DbCommit) | null;
  children?: ComponentChildren;
}) {
  return props.row?.url && props.row.commit ? (
    <a
      href={`${props.row.url.replace(/\/$/, "")}/commit/${props.row.commit}`}
      target="_blank"
      title={`${props.row.date} UTC`}
    >
      {props.children}
    </a>
  ) : (
    <>{props.children}</>
  );
}

interface DbSpacemanRev {
  spaceman_rev: string;
  date: string;
  describe: string;
}
interface DbSpacemanTag {
  name: string;
  spaceman_rev: string;
}
interface DbTree {
  tree: string;
  suite?: string;
  config?: string;
}
interface DbTreeAnalysis {
  tree: string;
  spaceman_rev: string;
  analysis: string;
}
interface DbCommit {
  commit: string;
  date: string;
  tree: string;
}
interface DbCodebase {
  codebase: string;
  url?: string;
}
interface DbCodebaseCommit {
  codebase: string;
  commit: string;
}

interface Panic {
  panic: string;
}

interface Analysis {
  precache_time: Duration;
  io_time: Duration;
  parse_time: Duration;
  parse_errors: ErrorCounts;
  fatal_errored: boolean;
  dreamchecker_time: Duration;
  dreamchecker_errors: ErrorCounts;
  dreamchecker_voluntary: boolean;
  errors: string;
}

interface Duration {
  secs: number;
  nanos: number;
}

interface ErrorCounts {
  error: number;
  warning: number;
  info: number;
  hint: number;
}
