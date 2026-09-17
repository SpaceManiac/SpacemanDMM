import { render } from "preact";
import { useEffect, useState } from "preact/hooks";

import { connectToDatabase } from "./db";
import "./zoo.css";

export interface ZooProps {}

export function renderZoo(element: HTMLElement, props: ZooProps = {}) {
  // if (element === document.body) {
  //   document.documentElement.classList.add(bem("html"));
  // }
  render(<Zoo {...props} />, element);
}

export function Zoo(props: ZooProps) {
  type Row = {
    codebase: string;
    git_rev?: string;
    suite?: string;
    config?: string;

    analysis: string;
    git_date: string;
    git_remote?: string;
  };
  const [data, setData] = useState<Row[]>([]);

  useEffect(() => {
    (async () => {
      const { db } = await connectToDatabase();
      const data = await db.query(`
        SELECT analysis.codebase, analysis.git_rev, git_remote, git_date, suite, config, spaceman_rev, analysis
        FROM analysis
        LEFT JOIN config
        ON analysis.codebase = config.codebase
          AND analysis.git_rev = config.git_rev
        -- WHERE spaceman_rev = :spaceman_rev
        ORDER BY git_date DESC
      `);
      setData(data as Row[]);
    })();
  }, []);

  return (
    <table>
      <thead>
        <tr>
          <th colspan={4}></th>
          <th colspan={7}>master</th>
        </tr>
        <tr>
          <th rowspan={2}>Codebase</th>
          <th rowspan={2}>Date</th>
          <th rowspan={2} colspan={2}>
            Config
          </th>

          <th rowspan={2}></th>
          <th colspan={3}>Parse</th>
          <th colspan={3}>Check</th>
        </tr>
        <tr>
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
        </tr>
      </thead>
      <tbody>
        {data.map((row) => {
          const analysis = JSON.parse(row.analysis);

          const date = row.git_date.substring(0, 10);
          const date2 = row.git_remote && row.git_rev;

          return (
            <tr>
              {/* Codebase part */}
              <td>{row.codebase}</td>

              <td>
                {row.git_remote && row.git_rev ? (
                  <a
                    href={`${row.git_remote.replace(/\/$/, "")}/commit/${row.git_rev}`}
                    target="_blank"
                  >
                    {date}
                  </a>
                ) : (
                  date
                )}
              </td>
              <td>
                {row.suite ? (
                  <a
                    href={`https://github.com/SpaceManiac/SpacemanDMM/releases/${row.suite}`}
                    target="_blank"
                  >
                    {row.suite}
                  </a>
                ) : null}
              </td>
              {/* Analysis part */}
              {analysis.dreamchecker_voluntary ? (
                <td class="icon" title="config: dreamchecker = true">
                  {CONFIG_DREAMCHECKER}
                </td>
              ) : row.config ? (
                <td class="icon" title="config present">
                  {CONFIG_MISC}
                </td>
              ) : (
                <td class="icon" title="no config"></td>
              )}
              <td class="icon">{LOG}</td>
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
            </tr>
          );
        })}
        {data.length === 0 ? (
          <tr>
            <td colspan={12}>Loading...</td>
          </tr>
        ) : null}
      </tbody>
    </table>
  );
}

function ErrorCount(props: { amount: number }) {
  return <td class="error-count">{props.amount || null}</td>;
}

const ERROR = "⛔";
const WARNING = "⚠️";
const INFO = "ℹ️";

const FATAL_PARSER_ERROR = ERROR;
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
