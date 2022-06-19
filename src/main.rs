use std::path::{
    Path,
    PathBuf,
};

use clap::{
    Parser,
    Subcommand,
};
use eyre::{
    Result,
    WrapErr,
};
#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    // /// Optional name to operate on
    // #[clap(value_parser)]
    // name: Option<String>,

    // /// Sets a custom config file
    // #[clap(short, long, value_parser, value_name = "FILE")]
    // config: Option<PathBuf>,

    // /// Turn debugging information on
    // #[clap(short, long, action = clap::ArgAction::Count)]
    // debug: u8,
    #[clap(subcommand)]
    command: Option<Commands>,
}
#[derive(Subcommand)]
enum Commands {
    // /// does testing things
    // Test {
    //     /// lists test values
    //     #[clap(short, long, action)]
    //     list: bool,
    // },
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
use serde::{
    Deserialize,
    Serialize,
};
#[derive(Serialize, Deserialize, Debug, Clone)]
struct SniperConfig {
    snippet_directories: Vec<PathBuf>,
}
pub mod yasnippet_parser {
    use std::collections::HashMap;

    use super::*;
    use nom::{
        branch::alt,
        bytes::complete::{
            take_until,
            take_until1,
            take_while,
        },
        combinator::opt,
        error::{
            ErrorKind,
            ParseError,
            VerboseError,
        },
        multi::{
            many0,
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
    use nom_supreme::final_parser::final_parser;
    use nom_supreme::parser_ext::ParserExt;
    use nom_supreme::tag::complete::tag;
    // #[derive(Debug, PartialEq)]
    // pub enum CustomError<I> {
    //     MyError,
    //     Nom(I, ErrorKind),
    //     WhitespaceAt(I),
    // }

    // impl<I> ParseError<I> for CustomError<I> {
    //     fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    //         CustomError::Nom(input, kind)
    //     }

    //     fn append(_: I, _: ErrorKind, other: Self) -> Self {
    //         other
    //     }
    // }
    // type Res<'input, T> = IResult<&'input str, T, CustomError<&'input str>>;
    type Res<'input, T> = IResult<&'input str, T, nom_supreme::error::ErrorTree<&'input str>>;

    #[derive(Debug, Clone)]
    pub struct TemplateMarker {
        pub key: u32,
    }

    #[derive(Debug, Clone)]
    pub struct Text<'content>(&'content str);

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
        // debug_assert!(.is_ok());

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
    fn snippet_text_rest<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        Ok(("", Text(content).into()))
    }

    fn snippet_text<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        let (content, text) = alt((
            take_until1("$"),
            // take_until("\n").context("no more template tags - drain to end of file"),
        ))(content)?; // TODO: brackets mode of yasnippet

        Ok((content, Text(text).into()))
    }

    fn snippet_part<'content>(content: &'content str) -> Res<'content, SnippetPart<'content>> {
        alt((
            snippet_text.context("snippet_text"),
            template_marker.context("template_marker"),
            // snippet_text_rest,
        ))(content)
    }

    impl<'content> Snippet<'content> {
        pub fn parse(content: &'content str) -> Result<Self> {
            // let header = final_parser(header);
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

fn main() -> Result<()> {
    let cli = Cli::parse();
    let config = SniperConfig::load()?;
    Ok(())
}
