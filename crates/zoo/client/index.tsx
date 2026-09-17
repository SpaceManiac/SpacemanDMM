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
        <CodebaseBySpaceman />
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

  const data = useQueryOne<AnalysisRow & ConfigRow>(
    `
      SELECT *
      FROM analysis
      LEFT JOIN config
      ON analysis.codebase = config.codebase
        AND analysis.git_rev = config.git_rev
      WHERE analysis.codebase = :codebase
        AND analysis.git_rev = :git_rev
        AND spaceman_rev = :spaceman_rev
    `,
    {
      ":codebase": props.codebase,
      ":git_rev": props.git_rev,
      ":spaceman_rev": props.spaceman_rev,
    },
  );

  const analysis: Analysis = data ? JSON.parse(data.analysis) : null;

  return (
    <>
      <h3>
        Run detail: {props.codebase}
        {data ? (
          <>
            {" "}
            <RevisionLink row={data}>
              {data.git_date.substring(0, 10)}
            </RevisionLink>
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
              <AnsiHtml text={analysis.errors.trimEnd()} />
            </pre>
          </fieldset>
        </>
      )}
    </>
  );
}

function RevisionDetail(props: { codebase: string; git_rev: string }) {
  const data = useQueryOne<AnalysisRow & ConfigRow>(
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
            <RevisionLink row={data}>
              {data.git_date.substring(0, 10)}
            </RevisionLink>
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

function CodebaseBySpaceman(props: { date_max?: string; date_min?: string }) {
  const tags = useSpacemanTags();

  const data = useQuery<AnalysisRow>(`
    SELECT *
    FROM analysis
    ORDER BY git_date DESC
  `);
  const configs = useQuery<ConfigRow>(`
    SELECT *
    FROM config
  `);

  const configMap = new Map(
    configs?.map((row) => [`${row.codebase}${row.git_rev}`, row]),
  );
  const spacemanRevSet = new Set(data?.map((row) => row.spaceman_rev));
  const filteredTags = tags.filter((tag) =>
    spacemanRevSet.has(tag.spaceman_rev),
  );

  return (
    <table>
      <thead>
        <tr>
          <th colspan={4}></th>
          {filteredTags.map((tag) => (
            <RunHeader1 {...tag} />
          ))}
        </tr>
        <tr>
          <th rowspan={2}>Codebase</th>
          <th rowspan={2}>Date</th>
          <th rowspan={2} colspan={2}>
            Config
          </th>
          {filteredTags.map(() => (
            <RunHeader2 />
          ))}
        </tr>
        <tr>
          {filteredTags.map(() => (
            <RunHeader3 />
          ))}
        </tr>
      </thead>
      <tbody>
        {data ? (
          data.map((row) => {
            const config = configMap.get(`${row.codebase}${row.git_rev}`) || {
              suite: undefined,
              config: undefined,
            };
            const analysis: Analysis = JSON.parse(row.analysis);

            const date = row.git_date.substring(0, 10);

            return (
              <tr>
                {/* Codebase part */}
                <td>{row.codebase}</td>
                <td>
                  {row.git_remote && row.git_rev ? (
                    <a
                      href={`${row.git_remote.replace(/\/$/, "")}/commit/${row.git_rev}`}
                      target="_blank"
                      title={`${row.git_date} UTC`}
                    >
                      {date}
                    </a>
                  ) : (
                    date
                  )}
                </td>
                <td>
                  {config.suite ? (
                    <a
                      href={`https://github.com/SpaceManiac/SpacemanDMM/releases/${config.suite}`}
                      target="_blank"
                    >
                      {config.suite}
                    </a>
                  ) : null}
                </td>
                <td class="icon">
                  {analysis.dreamchecker_voluntary ? (
                    <a
                      href={`#codebase=${row.codebase}&git_rev=${row.git_rev}`}
                      title="config: dreamchecker = true"
                    >
                      {CONFIG_DREAMCHECKER}
                    </a>
                  ) : config.config ? (
                    <a
                      href={`#codebase=${row.codebase}&git_rev=${row.git_rev}`}
                      title="config present"
                    >
                      {CONFIG_MISC}
                    </a>
                  ) : (
                    <a
                      href={`#codebase=${row.codebase}&git_rev=${row.git_rev}`}
                      title="no config"
                    >
                      {CONFIG_NONE}
                    </a>
                  )}
                </td>
                {/* Analysis part */}
                <RunEntry
                  codebase={row.codebase}
                  git_rev={row.git_rev || ""}
                  spaceman_rev={row.spaceman_rev}
                  analysis={analysis}
                />
              </tr>
            );
          })
        ) : (
          <tr>
            <td colspan={12}>Loading...</td>
          </tr>
        )}
      </tbody>
    </table>
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
  analysis: any;
}) {
  const { codebase, git_rev, spaceman_rev, analysis } = props;
  return (
    <>
      <td class="icon">
        <a
          href={`#codebase=${codebase}&git_rev=${git_rev}&spaceman_rev=${spaceman_rev}`}
          title="run details"
        >
          {LOG}
        </a>
      </td>
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
    useQuery(
      `SELECT name, spaceman_rev FROM spaceman_tag ORDER BY sort DESC`,
    ) || []
  );
}

function RevisionLink(props: {
  row?: AnalysisRow | null;
  children?: ComponentChildren;
}) {
  return props.row?.git_remote && props.row.git_rev ? (
    <a
      href={`${props.row.git_remote.replace(/\/$/, "")}/commit/${props.row.git_rev}`}
    >
      {props.children}
    </a>
  ) : (
    <>{props.children}</>
  );
}

type ConfigRow = {
  codebase: string;
  git_rev: string;
  suite?: string;
  config?: string;
};

type AnalysisRow = {
  codebase: string;
  spaceman_rev: string;
  git_remote?: string;
  git_rev?: string;
  git_date: string;
  analysis: string;
};

type Analysis = {
  precache_time: Duration;
  io_time: Duration;
  parse_time: Duration;
  parse_errors: ErrorCounts;
  fatal_errored: boolean;
  dreamchecker_time: Duration;
  dreamchecker_errors: ErrorCounts;
  dreamchecker_voluntary: boolean;
  errors: string;
};

type Duration = {
  secs: number;
  nanos: number;
};

type ErrorCounts = {
  error: number;
  warning: number;
  info: number;
  hint: number;
};
