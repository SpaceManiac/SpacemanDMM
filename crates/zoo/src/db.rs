use std::{error::Error, ops::Deref, path::Path};

use git2::{Commit, DescribeFormatOptions, DescribeOptions, Repository};
use rusqlite::{Connection, named_params};

use crate::format_git_time;

const SCHEMA: &'static str = r#"
CREATE TABLE IF NOT EXISTS "spaceman_rev" (
	"spaceman_rev"	TEXT NOT NULL,
	"date"	TEXT NOT NULL,
	"describe"	TEXT NOT NULL,
	PRIMARY KEY("spaceman_rev")
);
CREATE TABLE IF NOT EXISTS "spaceman_tag" (
	"name"	TEXT NOT NULL,
	"spaceman_rev"	TEXT NOT NULL,
	PRIMARY KEY("name") ON CONFLICT REPLACE,
	UNIQUE("spaceman_rev"),
    FOREIGN KEY("spaceman_rev") REFERENCES "spaceman_rev"("spaceman_rev")
);
CREATE TABLE IF NOT EXISTS "tree" (
	"tree"	TEXT NOT NULL,
	"suite"	TEXT,
	"config"	TEXT,
	PRIMARY KEY("tree")
);
CREATE TABLE IF NOT EXISTS "tree_analysis" (
	"tree"	TEXT NOT NULL,
	"spaceman_rev"	TEXT NOT NULL,
	"analysis"	TEXT NOT NULL,
	PRIMARY KEY("tree","spaceman_rev"),
	FOREIGN KEY("spaceman_rev") REFERENCES "spaceman_rev"("spaceman_rev"),
	FOREIGN KEY("tree") REFERENCES "tree"("tree")
);
CREATE TABLE IF NOT EXISTS "commit" (
	"commit"	TEXT NOT NULL,
	"date"	TEXT NOT NULL,
	"tree"	TEXT NOT NULL,
	PRIMARY KEY("commit"),
	FOREIGN KEY("tree") REFERENCES "tree"("tree")
);
CREATE TABLE IF NOT EXISTS "codebase" (
	"codebase"	TEXT NOT NULL,
	"url"	TEXT,
	PRIMARY KEY("codebase")
);
CREATE TABLE IF NOT EXISTS "codebase_commit" (
	"codebase"	TEXT NOT NULL,
	"commit"	TEXT NOT NULL,
	FOREIGN KEY("codebase") REFERENCES "codebase"("codebase"),
	FOREIGN KEY("commit") REFERENCES "commit"("commit")
);
"#;

pub struct Db(Connection);

impl Db {
    pub fn open(zoo: &Path) -> Self {
        let path = zoo.join("zoo.db");
        let conn = Connection::open(&path).unwrap();
        conn.execute_batch(SCHEMA).unwrap();
        Db(conn)
    }

    // ----------------------------------------------------------------------------
    // spaceman_rev and spaceman_tag tables

    pub fn set_spaceman_tags(&self) -> Result<(), Box<dyn Error>> {
        let repo = Repository::open_from_env()?;

        let master = repo
            .find_branch("master", git2::BranchType::Local)?
            .into_reference()
            .peel_to_commit()?;
        let spaceman_rev = self.insert_spaceman_rev_commit(&master)?;
        self.insert_spaceman_tag("master", &spaceman_rev).unwrap();
        eprintln!("master = {}", &spaceman_rev[..12]);

        repo.tag_foreach(|id, name| {
            let name = str::from_utf8(name).unwrap();
            let name = name.strip_prefix("refs/tags/").unwrap_or(name);
            if name.starts_with("suite-") {
                let commit = repo
                    .find_tag(id)
                    .unwrap()
                    .peel()
                    .unwrap()
                    .into_commit()
                    .unwrap();
                let spaceman_rev = self.insert_spaceman_rev_commit(&commit).unwrap();
                self.insert_spaceman_tag(name, &spaceman_rev).unwrap();
            }
            true
        })?;

        let head = repo.head()?.peel_to_commit()?;
        let base = repo.merge_base(master.id(), head.id())?;
        let base = repo.find_commit(base)?;
        if base.id() != master.id() {
            let spaceman_rev = self.insert_spaceman_rev_commit(&base)?;
            eprintln!("HEAD = {}", &spaceman_rev[..12]);
        }

        Ok(())
    }

    fn insert_spaceman_rev_commit(&self, commit: &Commit) -> Result<String, Box<dyn Error>> {
        let spaceman_rev = commit.id().to_string();
        let date = format_git_time(commit.time());
        let describe = commit
            .as_object()
            .describe(&DescribeOptions::new())?
            .format(Some(&DescribeFormatOptions::new().abbreviated_size(12)))?;
        self.insert_spaceman_rev(&spaceman_rev, &date, &describe)?;
        Ok(spaceman_rev)
    }

    fn insert_spaceman_rev(
        &self,
        spaceman_rev: &str,
        date: &str,
        describe: &str,
    ) -> Result<(), Box<dyn Error>> {
        self.0.execute(
            "
                INSERT INTO spaceman_rev (spaceman_rev, date, describe)
                VALUES (:spaceman_rev, :date, :describe)
                ON CONFLICT DO UPDATE SET describe = :describe
            ",
            named_params! {
                ":spaceman_rev": spaceman_rev,
                ":date": date,
                ":describe": describe,
            },
        )?;
        Ok(())
    }

    fn insert_spaceman_tag(&self, name: &str, spaceman_rev: &str) -> Result<(), Box<dyn Error>> {
        self.0.execute(
            "
            INSERT INTO spaceman_tag (name, spaceman_rev)
            VALUES (:name, :spaceman_rev)
            ON CONFLICT DO UPDATE SET name = :name, spaceman_rev = :spaceman_rev
        ",
            named_params! { ":name": name, ":spaceman_rev": spaceman_rev },
        )?;
        Ok(())
    }

    // ----------------------------------------------------------------------------
    // tree table

    //pub fn insert_tree_with(&self, tree: &str, f: impl FnOnce() -> (String, String)) -> bool {}
}

impl Deref for Db {
    type Target = Connection;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
