use serde_json::Value;

pub fn parse_json_array<T: std::convert::From<i64>>(array: &[Value]) -> Vec<Option<T>> {
    array
        .iter()
        .map(|item| match item {
            Value::Number(n) => {
                if let Some(num) = n.as_i64() {
                    Some(num.into())
                } else {
                    panic!("Unexpected non-integer number found")
                }
            }
            Value::Null => None, // Write None for Null value
            _ => panic!("Unexpected variant encountered"),
        })
        .collect::<Vec<Option<T>>>()
}
