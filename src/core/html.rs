use chrono::Utc;

use super::parser::Parser;

#[derive(Default)]
pub struct HTML<'r> {
    // remote: &'r str,
    parser: Option<&'r Parser<'r>>,
}

#[derive(Debug)]
pub enum HTMLError {
    UninitializedError,
}

impl<'r> HTML<'r> {
    pub fn init(&mut self, parser: &'r Parser<'r>) {
        self.parser = Some(parser);
    }

    fn head() -> String {
        "<head><style>body {color: black; background-color: white;} tr:hover { background: yellow }</style></head>\n".to_string()
    }

    #[rustfmt::skip]
    fn headers_table(&self) -> Result<String, HTMLError> {
        if self.parser.is_none() {
            return Err(HTMLError::UninitializedError);
        }

        let headers = self.parser.unwrap().header_lines();

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

        Ok(table.join(""))
    }

    fn method(&self) -> Result<String, HTMLError> {
        if self.parser.is_none() {
            return Err(HTMLError::UninitializedError);
        }

        Ok(format!("<h2>{}</h2>", self.parser.unwrap().method()))
    }

    fn raw(&self) -> Result<String, HTMLError> {
        if self.parser.is_none() {
            return Err(HTMLError::UninitializedError);
        }

        Ok(format!(
            r#"<div mini:hint="folded;Raw Request"><h2>Raw request</h2><pre>{}</pre></div>"#,
            self.parser.unwrap().raw()
        ))
    }

    fn time() -> String {
        let dt = Utc::now();
        let formatted_date = dt.format("%a %b %e %T %Y").to_string();

        format!("<p>Page generated at {}</p>", formatted_date)
    }

    fn body(&self) -> Result<String, HTMLError> {
        Ok(format!(
            r#"<body>{}{}<hr noshade="">{}<hr noshade="">{}</body>"#,
            self.method()?,
            self.headers_table()?,
            self.raw()?,
            Self::time()
        ))
    }

    pub fn document(&self) -> Result<String, HTMLError> {
        let head = HTML::head();
        let body = self.body()?;

        Ok(format!("<html>{}{}</html>", head, body))
    }
}
