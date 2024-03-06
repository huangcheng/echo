use chrono::Utc;

use super::parser::Parser;

pub struct HTML<'r> {
    remote: &'r str,
    parser: &'r Parser<'r>,
}

impl<'r> HTML<'r> {
    pub fn new(remote: &'r str, parser: &'r Parser<'r>) -> Self {
        HTML { remote, parser }
    }

    fn head() -> String {
        "<head><style>body {color: black; background-color: white;} tr:hover { background: yellow }</style></head>\n".to_string()
    }

    #[rustfmt::skip]
    fn headers_table(&self) -> String {
        let headers = self.parser.header_lines();

        let mut table = vec![String::from(
            r#"<table mini:hint="folded;Headers" border="0" cellpadding="3" cellspacing="0"><tbody>"#,
        )];

        for header in headers {
            if header.len() == 2 {
                let key = header[0].as_str();
                let value = header[1].as_str();

                let key = format!("{}:", key);

                let row = [
                    "<tr>",
                        r#"<td valign="top">"#,
                            "<b>",
                                key.as_str(),
                            "</b>",
                        "</td>",
                        "<td>",
                            value,
                        "</td>",
                    "</tr>",
                ];

                let row = row.join("");

                table.push(row);
            }
        }

        table.push(String::from("</tbody></table>"));

        table.join("")
    }

    fn method(&self) -> String {
        format!("<h2>{}</h2>", self.parser.method())
    }

    fn remote(&self) -> String {
        format!("<h2>Remote: {}</h2>", self.remote)
    }

    fn raw(&self) -> String {
        format!(
            r#"<div mini:hint="folded;Raw Request"><h2>Raw request</h2><pre>{}</pre></div>"#,
            self.parser.raw()
        )
    }

    fn time() -> String {
        let dt = Utc::now();
        let formatted_date = dt.format("%a %b %e %T %Y").to_string();

        format!("<p>Page generated at {}</p>", formatted_date)
    }

    fn body(&self) -> String {
        format!(
            r#"<body>{}{}{}<hr noshade="">{}<hr noshade="">{}</body>"#,
            self.method(),
            self.remote(),
            self.headers_table(),
            self.raw(),
            Self::time()
        )
    }

    pub fn document(&self) -> String {
        let head = HTML::head();
        let body = self.body();

        format!("<html>{}{}</html>", head, body)
    }
}
