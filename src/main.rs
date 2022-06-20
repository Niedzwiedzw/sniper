use std::{
    collections::HashMap,
    path::{
        Path,
        PathBuf,
    },
};

use clap::{
    Parser,
    Subcommand,
};
use eyre::{
    Result,
    WrapErr,
};

/// CLI code snippet manager inspired by YASNippet.
/// Config resides in ~/.config/sniper, and is generated on first launch
#[derive(Parser)]
#[clap(author, version)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}
#[derive(Subcommand)]
enum Commands {
    /// renders the template onto the terminal
    Fetch {
        /// variables to fill the template with
        #[clap(short, long)]
        values: Vec<String>,
        /// [key:] to find the template by
        #[clap(short, long)]
        key: String,
    },
}

fn make_sure_dir_exists(path: &Path) -> Result<()> {
    if !path.exists() {
        std::fs::create_dir_all(&path)
            .wrap_err_with(|| format!("creating a required {path:?} directory"))?;
    }
    if !path.is_dir() {
        eyre::bail!("{path:?} exists but is not a directory");
    }
    Ok(())
}

fn config_dir() -> Result<PathBuf> {
    let config_dir = directories::ProjectDirs::from("com", "niedzwiedz", "sniper")
        .ok_or_else(|| eyre::eyre!("could not initialize project directories"))
        .map(|d| d.config_dir().to_owned())?;
    make_sure_dir_exists(&config_dir)?;
    Ok(config_dir)
}
use itertools::Itertools;

use serde::{
    Deserialize,
    Serialize,
};

use yasnippet_parser::{
    Snippet,
    TemplateMarker,
};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SniperConfig {
    snippet_directories: Vec<PathBuf>,
}

pub mod yasnippet_parser {
    use std::collections::HashMap;

    use super::*;
    use nom::{
        branch::alt,
        bytes::complete::{
            take_until1,
            take_while,
        },
        combinator::opt,
        multi::{
            many1,
            many_till,
        },
        sequence::{
            delimited,
            preceded,
            separated_pair,
            terminated,
            tuple,
        },
        IResult,
        Parser,
    };
    use nom_supreme::parser_ext::ParserExt;
    use nom_supreme::tag::complete::tag;

    type Res<'input, T> = IResult<&'input str, T, nom_supreme::error::ErrorTree<&'input str>>;

    #[derive(Debug, Clone)]
    pub struct TemplateMarker {
        pub key: u32,
    }

    #[derive(Debug, Clone)]
    pub struct Text<'content>(pub &'content str);

    #[derive(Debug, Clone, derive_more::From)]
    pub enum SnippetPart<'content> {
        Text(Text<'content>),
        Template(TemplateMarker),
    }
    #[derive(Debug, Clone)]
    pub struct Header<'content> {
        pub name: &'content str,
        pub key: &'content str,
    }
    #[derive(Debug, Clone)]
    pub struct Snippet<'content> {
        pub header: Header<'content>,
        pub parts: Vec<SnippetPart<'content>>,
    }

    pub fn text(input: &str) -> Res<&str> {
        static ILLEGAL: &[char] = &[':'];
        take_while(|v: char| !v.is_whitespace() && !ILLEGAL.contains(&v)).parse(input)
    }
    pub fn whitespace(input: &str) -> Res<&str> {
        take_while(|v: char| v.is_whitespace() && v != '\n').parse(input)
    }
    type SnippetParams<'a> = HashMap<&'a str, &'a str>;

    pub fn header<'content>(content: &'content str) -> Res<SnippetParams<'content>> {
        let hash_prefix = || terminated(tag("#"), whitespace).context("hash prefix");
        let end_of_line = || {
            tuple((
                opt(whitespace).context("optional whitespace"),
                tag("\n").context("newline"),
            ))
            .context("end of line")
        };
        let at_least_one_newline = || many1(end_of_line()).context("at least one newline");
        static HEADER_START: &str = "-*- mode: snippet -*-";
        let start_line = || {
            terminated(
                preceded(hash_prefix(), tag(HEADER_START).context(HEADER_START)),
                at_least_one_newline().context("start_line :: end of line"),
            )
        };
        let colon = || delimited(opt(whitespace), tag(":"), opt(whitespace));
        let key_value = || separated_pair(text, colon(), text);
        let key_value_entry = || {
            terminated(preceded(hash_prefix(), key_value()), at_least_one_newline())
                .context("header key-values")
        };
        #[cfg(debug_assertions)]
        {
            key_value()
                .parse("name: vuesinglefile3")
                .expect("key_value works");

            key_value_entry()
                .parse("# name: vuesinglefile3\n")
                .expect("key_value_entry works");
        }

        let end_of_config = terminated(preceded(hash_prefix(), tag("--")), end_of_line())
            .context("header termination");
        let params = || {
            terminated(
                many_till(key_value_entry(), end_of_config),
                at_least_one_newline(),
            )
        };

        let (content, _whitespace) = opt(at_least_one_newline()).parse(content)?;

        let (content, _marker) = start_line().parse(content)?;
        let (content, (params, _)) = params().parse(content)?;

        let params: HashMap<&str, &str> = params.into_iter().collect();
        Ok((content, params))
    }

    fn template_marker<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        let (content, key) = preceded(
            tag("$"),
            take_while(|c: char| c.is_numeric()).map_res(|s: &str| s.parse::<u32>()),
        )(content)?;

        Ok((content, TemplateMarker { key }.into()))
    }
    #[allow(dead_code)]
    fn snippet_text_rest<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        Ok(("", Text(content).into()))
    }

    fn snippet_text<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        let (content, text) = alt((take_until1("$"),))(content)?; // TODO: brackets mode of yasnippet

        Ok((content, Text(text).into()))
    }

    fn snippet_part<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        alt((
            snippet_text.context("snippet_text"),
            template_marker.context("template_marker"),
        ))(content)
    }

    impl<'content> Snippet<'content> {
        pub fn parse(content: &'content str) -> Result<Self> {
            let (content, header_values) =
                header(content).map_err(|e| eyre::eyre!("parsing header:\n{e:#?}"))?;
            let header = Header {
                name: header_values
                    .get("name")
                    .ok_or_else(|| eyre::eyre!("header value missing: "))?,
                key: header_values
                    .get("key")
                    .ok_or_else(|| eyre::eyre!("header value missing: "))?,
            };

            let all_parts = many1(snippet_part);
            let (content, mut parts) = all_parts
                .context("all parts of template")
                .parse(content)
                .map_err(|e| eyre::eyre!("parsing template:\n{e:#?}"))?;
            parts.push(SnippetPart::Text(Text(content)));
            Ok(Self { header, parts })
        }
    }
    #[cfg(test)]
    mod tests {
        use super::*;

        const VUE_SNIPPET_CONTENT: &str = include_str!("../test_data/vue3");
        #[test]
        fn test_vue_snippet_manual() -> Result<()> {
            let mut manual_parser = tuple((
                header,
                snippet_text,
                template_marker,
                snippet_text,
                template_marker,
                template_marker,
                snippet_text,
                template_marker,
                snippet_text,
                template_marker,
                snippet_text_rest,
            ));
            let (content, _) = manual_parser.parse(VUE_SNIPPET_CONTENT)?;
            assert_eq!(content, "");
            Ok(())
        }
        #[test]
        fn test_vue_snippet() -> Result<()> {
            let snippet = Snippet::parse(VUE_SNIPPET_CONTENT).wrap_err("failed to parse")?;
            assert_eq!(snippet.header.key, "vue");
            assert_eq!(snippet.header.name, "vuesinglefile3");
            println!("{snippet:#?}");
            Ok(())
        }

        #[test]
        fn test_snippet_text_grabbing() -> Result<()> {
            let content = r#"<template>
  <div class="$1
"#;
            let (_, parsed) = snippet_text.parse(content)?;
            match parsed {
                SnippetPart::Text(Text(text)) => assert_eq!(text, content.trim_end_matches("$1\n")),
                SnippetPart::Template(_) => panic!("nope:\n{parsed:#?}"),
            }
            Ok(())
        }

        #[test]
        fn test_template_marker() -> Result<()> {
            let (_, marker) = template_marker("$1")?;
            match marker {
                SnippetPart::Template(TemplateMarker { key }) => {
                    assert_eq!(key, 1);
                    Ok(())
                }
                _ => {
                    panic!("bad marker {marker:#?}");
                }
            }
        }
    }
}
impl SniperConfig {
    fn generate_default() -> Result<Self> {
        let default_directory = config_dir()?.join("snippets");
        make_sure_dir_exists(&default_directory)?;
        Ok(Self {
            snippet_directories: vec![default_directory],
        })
    }
    pub fn load() -> Result<Self> {
        let config_path = config_dir()?.join("sniper-config.toml");
        if !config_path.exists() {
            let config = Self::generate_default()?;
            let content =
                toml::to_string_pretty(&config).wrap_err("serializing default config content")?;
            std::fs::write(&config_path, &content).wrap_err("writing default config content")?;
            Ok(config)
        } else {
            std::fs::read_to_string(&config_path)
                .wrap_err_with(|| format!("reading config from [{config_path:?}]"))
                .and_then(|content| toml::from_str(&content).wrap_err("deserializing config"))
        }
    }
}
#[derive(Debug, Clone)]
pub struct Sniper {
    pub config: SniperConfig,
}

impl Sniper {
    pub fn new(config: SniperConfig) -> Self {
        Self { config }
    }
    fn all_files_in_path(path: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
        let paths = walkdir::WalkDir::new(path)
            .into_iter()
            .filter_map(|e| e.ok())
            .map(|e| e.path().to_owned())
            .filter(|path| path.is_file())
            .filter_map(|path| path.canonicalize().ok());
        Box::new(paths)
    }

    fn all_snippet_paths<'sniper>(&'sniper self) -> Box<dyn Iterator<Item = PathBuf> + 'sniper> {
        let paths = self
            .config
            .snippet_directories
            .iter()
            .map(|p| p.as_ref())
            .flat_map(Self::all_files_in_path);
        Box::new(paths)
    }

    fn find_snippet_path(&self, key: &str) -> Option<PathBuf> {
        self.all_snippet_paths().into_iter().find_map(|path| {
            match std::fs::read_to_string(&path)
                .wrap_err_with(|| format!("reading snippet contents from [{path:?}]"))
            {
                Ok(content) => {
                    tracing::debug!("succesfully read contents of [{path:?}]");
                    match Snippet::parse(&content) {
                        Ok(snippet) if snippet.header.key == key => Some(path),
                        Err(e) => {
                            tracing::warn!("parsing a snippet at [{path:?}] :: {e:?}");
                            None
                        }
                        _ => None,
                    }
                }
                Err(e) => {
                    tracing::warn!("reading snippet content at [{path:?}] :: {e:?}");
                    None
                }
            }
        })
    }

    fn render_snippet(path: &Path, values: &[&str]) -> Result<String> {
        let content = std::fs::read_to_string(path)
            .wrap_err_with(|| format!("reading snippet at [{path:?}]"))?;
        let snippet =
            Snippet::parse(&content).wrap_err_with(|| format!("parsing snippet at [{path:?}]"))?;

        let mut values = values
            .iter()
            .enumerate()
            .map(|(index, value)| ((index + 1) as u32, *value))
            .collect::<HashMap<u32, &str>>();

        let slots = snippet
            .parts
            .iter()
            .filter_map(|part| match part {
                yasnippet_parser::SnippetPart::Text(_) => None,
                yasnippet_parser::SnippetPart::Template(TemplateMarker { key }) => Some(key),
            })
            .unique()
            .filter_map(|key| values.remove(key).map(|value| (*key, value)))
            .collect::<HashMap<u32, &str>>();
        let rendered = snippet
            .parts
            .iter()
            .filter(|part| match part {
                yasnippet_parser::SnippetPart::Text(_) => true,
                yasnippet_parser::SnippetPart::Template(TemplateMarker { key }) => *key != 0,
            })
            .map(|part| -> Result<&str> {
                match part {
                    yasnippet_parser::SnippetPart::Text(yasnippet_parser::Text(text)) => Ok(text),
                    yasnippet_parser::SnippetPart::Template(TemplateMarker { key }) => slots
                        .get(&key)
                        .map(|v| *v)
                        .ok_or_else(|| eyre::eyre!("no value for key [${key}] found")),
                }
            })
            .collect::<Result<Vec<_>>>()?
            .join("");
        Ok(rendered)
    }

    pub fn find_and_render_snippet(&self, key: &str, values: &[&str]) -> Result<String> {
        self.find_snippet_path(key)
            .ok_or_else(|| eyre::eyre!("finding snippet using key [{key}]"))
            .and_then(|path| Self::render_snippet(&path, values))
            .wrap_err_with(|| format!("rendering snippet for key [{key}] and values [{values:?}]"))
    }
}
use tracing_subscriber::{
    fmt,
    prelude::__tracing_subscriber_SubscriberExt,
    EnvFilter,
};
fn main() -> Result<()> {
    let logs_dir = config_dir()?.join("log");
    let file_appender = tracing_appender::rolling::daily(&logs_dir, "log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);
    let subscriber = tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(fmt::Layer::new().with_writer(std::io::stdout))
        .with(
            fmt::Layer::new()
                .compact()
                .with_ansi(false)
                .with_writer(non_blocking),
        );
    tracing::subscriber::set_global_default(subscriber)
        .context("Unable to set a global subscriber")?;
    let cli = Cli::parse();
    let config = SniperConfig::load()?;
    let sniper = Sniper::new(config);
    match cli.command {
        Commands::Fetch { values, key } => {
            let rendered = sniper.find_and_render_snippet(
                &key,
                &values.iter().map(|v| v.as_str()).collect::<Vec<_>>(),
            )?;
            println!("{rendered}");
        }
    };
    Ok(())
}
