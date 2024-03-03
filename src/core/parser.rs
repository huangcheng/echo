use std::collections::HashMap;

pub struct Parser<'r> {
    data: &'r [u8],
    size: usize,
}

impl<'r> Parser<'r> {
    pub fn new(data: &'r [u8], size: usize) -> Parser<'r> {
        Parser { data, size }
    }

    pub fn headers(&self) -> HashMap<String, String> {
        let mut headers = HashMap::new();

        let request = String::from_utf8_lossy(self.data);
        let lines = request.split("\r\n");
        let lines = lines.filter(|line| !line.is_empty());

        for line in lines {
            let parts: Vec<&str> = line.split(": ").collect();

            if parts.len() == 2 {
                let key = parts[0].to_string();
                let value = parts[1].to_string();

                headers.insert(key, value);
            }
        }

        headers
    }

    pub fn header_lines(&self) -> Vec<Vec<String>> {
        let mut headers = vec![];

        let request = String::from_utf8_lossy(self.data);
        let lines = request.split("\r\n");
        let lines = lines.filter(|line| !line.is_empty());

        for line in lines {
            let parts: Vec<&str> = line.split(": ").collect();

            if parts.len() == 2 {
                let key = parts[0].to_string();
                let value = parts[1].to_string();

                headers.push(vec![key, value]);
            }
        }

        headers
    }

    pub fn method(&self) -> String {
        let request = String::from_utf8_lossy(self.data);
        let lines = request.split("\r\n");
        let lines = lines.filter(|line| !line.is_empty());

        let first_line = lines.clone().next().unwrap();
        let parts: Vec<&str> = first_line.split(' ').collect();

        parts.join(" ").to_string()
    }

    pub fn raw(&self) -> String {
        let mut result = vec![];

        const ELEMENT_PER_LINE: usize = 0x10;

        let lines = self.size / ELEMENT_PER_LINE;
        let remainder = self.size % ELEMENT_PER_LINE;

        let lines = if remainder > 0 { lines + 1 } else { lines };

        for i in 0..lines {
            let start = i * ELEMENT_PER_LINE;
            let end = if i == lines - 1 {
                start + remainder
            } else {
                start + ELEMENT_PER_LINE
            };

            let line = &self.data[start..end];

            let hex = line
                .iter()
                .map(|b| format!("{:02x}", b))
                .collect::<Vec<String>>()
                .join(" ");
            let ascii = line
                .iter()
                .map(|b| {
                    if *b >= 32 && *b <= 126 {
                        *b as char
                    } else {
                        '.'
                    }
                })
                .collect::<String>();

            result.push(format!("{:3x}: {:47}   {}", start, hex, ascii));
        }

        result.join("\n")
    }
}
