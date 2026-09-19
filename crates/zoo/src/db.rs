use std::{error::Error, ops::Deref, path::Path};

use git2::{Commit, DescribeFormatOptions, DescribeOptions, Repository};
use rusqlite::{Connection, OptionalExtension, named_params};

use crate::{ABBREV, format_git_time};

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
	UNIQUE("codebase","commit"),
	FOREIGN KEY("codebase") REFERENCES "codebase"("codebase"),
	FOREIGN KEY("commit") REFERENCES "commit"("commit")
);
"#;

pub struct Db(Connection);

impl Db {
    pub fn open(root: &Path) -> Self {
        let conn = Connection::open(&root.join("zoo.db")).unwrap();
        conn.execute_batch(SCHEMA).unwrap();
        Db(conn)
    }

    // ----------------------------------------------------------------------------
    // spaceman_rev and spaceman_tag tables

    pub fn set_spaceman_tags(&self) -> Result<String, Box<dyn Error>> {
        let repo = Repository::open_from_env()?;

        let master = repo
            .find_branch("master", git2::BranchType::Local)?
            .into_reference()
            .peel_to_commit()?;
        let spaceman_rev = self.insert_spaceman_rev_commit(&master)?;
        self.insert_spaceman_tag("master", &spaceman_rev).unwrap();
        eprintln!("master = {}", &spaceman_rev[..ABBREV]);

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
        let spaceman_rev = self.insert_spaceman_rev_commit(&base)?;
        eprintln!("HEAD = {}", &spaceman_rev[..ABBREV]);
        Ok(spaceman_rev)
    }

    fn insert_spaceman_rev_commit(&self, commit: &Commit) -> Result<String, Box<dyn Error>> {
        let spaceman_rev = commit.id().to_string();
        let date = format_git_time(commit.time());
        let describe = commit
            .as_object()
            .describe(&DescribeOptions::new())?
            .format(Some(
                &DescribeFormatOptions::new().abbreviated_size(ABBREV as u32),
            ))?;
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
    // codebase, tree, and commit

    pub fn insert_codebase(&self, codebase: &str, url: Option<&str>) -> Result<(), Box<dyn Error>> {
        self.0.execute(
            "
                INSERT INTO codebase (codebase, url)
                VALUES (:codebase, :url)
                ON CONFLICT DO UPDATE SET url = :url
            ",
            named_params! {
                ":codebase": codebase,
                ":url": url,
            },
        )?;
        Ok(())
    }

    pub fn insert_tree_with(
        &self,
        tree: &str,
        f: impl FnOnce() -> (Option<String>, Option<String>),
    ) -> Result<(), Box<dyn Error>> {
        let existing_row = self
            .0
            .query_one(
                "SELECT tree FROM tree WHERE tree = :tree",
                named_params! {
                    ":tree": tree,
                },
                |_| Ok(()),
            )
            .optional()?;

        if existing_row.is_none() {
            let (suite, config) = f();
            self.0.execute(
                "
                    INSERT INTO tree (tree, suite, config)
                    VALUES (:tree, :suite, :config)
                    ON CONFLICT DO NOTHING
                ",
                named_params! {
                    ":tree": tree,
                    ":suite": suite,
                    ":config": config,
                },
            )?;
        }

        Ok(())
    }

    pub fn insert_commit(&self, commit: &Commit) -> Result<(), Box<dyn Error>> {
        let tree = commit.as_object().peel_to_tree()?.id().to_string();
        let date = format_git_time(commit.time());
        let commit = commit.id().to_string();
        self.0.execute(
            r#"
                INSERT INTO "commit" ("commit", date, tree)
                VALUES (:commit, :date, :tree)
                ON CONFLICT DO NOTHING
            "#,
            named_params! {
                ":commit": commit,
                ":date": date,
                ":tree": tree,
            },
        )?;
        Ok(())
    }

    pub fn insert_codebase_commit(
        &self,
        codebase: &str,
        commit: &str,
    ) -> Result<(), Box<dyn Error>> {
        self.0.execute(
            r#"
                INSERT INTO codebase_commit (codebase, "commit")
                VALUES (:codebase, :commit)
                ON CONFLICT DO NOTHING
            "#,
            named_params! {
                ":codebase": codebase,
                ":commit": commit,
            },
        )?;
        Ok(())
    }

    // ----------------------------------------------------------------------------
    // analysis

    pub fn insert_tree_analysis_with(
        &self,
        tree: &str,
        spaceman_rev: &str,
        f: impl FnOnce() -> String,
    ) -> Result<(), Box<dyn Error>> {
        let existing_row = self
            .0
            .query_one(
                "
                    SELECT tree FROM tree_analysis
                    WHERE tree = :tree
                    AND spaceman_rev = :spaceman_rev
                ",
                named_params! {
                    ":tree": tree,
                    ":spaceman_rev": spaceman_rev,
                },
                |_| Ok(()),
            )
            .optional()?;

        if existing_row.is_none() {
            let analysis = f();
            self.0.execute(
                "
                    INSERT INTO tree_analysis (tree, spaceman_rev, analysis)
                    VALUES (:tree, :spaceman_rev, :analysis)
                    ON CONFLICT DO UPDATE SET analysis = :analysis
                ",
                named_params! {
                    ":tree": tree,
                    ":spaceman_rev": spaceman_rev,
                    ":analysis": analysis,
                },
            )?;
        }

        Ok(())
    }
}

impl Deref for Db {
    type Target = Connection;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
