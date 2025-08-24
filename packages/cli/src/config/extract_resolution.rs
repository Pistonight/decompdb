use cu::pre::*;
use regex::Regex;

/// Config for name resolution for the extract command
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgExtractResolution {
    /// Rules for name resolutions
    pub rules: CfgExtractResolutionRules,
    /// Tests for the rules. The first name should be preferred over the second
    #[serde(default)]
    pub test: Vec<(String, String)>,
}

impl CfgExtractResolution {
    /// Validate the rules
    pub fn test_rules(&self) -> cu::Result<()> {
        let mut has_errors = false;
        for (more_preferred, less_preferred) in &self.test {
            let k1 = self.rules.get_sort_key(more_preferred);
            let k2 = self.rules.get_sort_key(less_preferred);
            match k1.cmp(&k2) {
                std::cmp::Ordering::Less => {}
                std::cmp::Ordering::Equal => {
                    has_errors = true;
                    cu::error!(
                        "name resolution rule test failed: left == right\n  left: {more_preferred}\n  right: {less_preferred}\n  - expected left to be more preferred, but they are equal."
                    );
                }
                std::cmp::Ordering::Greater => {
                    has_errors = true;
                    cu::error!(
                        "name resolution rule test failed: left < right\n  left: {more_preferred}\n  right: {less_preferred}\n  - expected left to be more preferred, but it is less preferred."
                    );
                }
            }
        }
        if has_errors {
            cu::bail!("name resolution rule test failed");
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct CfgExtractResolutionRules {
    /// Pattern for preferrence, from more preferred to less preferred
    pub prefer: Vec<Regex>,
    /// Pattern for dislikeness, from less disliked to more disliked
    pub dislike: Vec<Regex>,
}

impl CfgExtractResolutionRules {
    /// Get a sort key that can be used to sort the name from most preferred to least preferred
    pub fn get_sort_key(&self, name: &str) -> usize {
        let prefer_i = self
            .prefer
            .iter()
            .position(|x| x.is_match(name))
            .unwrap_or(self.prefer.len());
        let dislike_i = self.dislike.iter().position(|x| x.is_match(name)).unwrap_or(0);

        prefer_i << 16 | dislike_i
    }
}

impl<'de> Deserialize<'de> for CfgExtractResolutionRules {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        return deserializer.deserialize_seq(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = CfgExtractResolutionRules;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "an array of name regular expressions")
            }
            fn visit_seq<A: serde::de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                const MAX: usize = 60000;
                let mut prefer = vec![];
                let mut dislike = vec![];
                let mut is_parsing_prefer = true;
                while let Some(s) = seq.next_element::<&str>()? {
                    if s == "<default>" {
                        is_parsing_prefer = false;
                        continue;
                    }
                    let r = match Regex::new(s) {
                        Err(e) => {
                            return Err(serde::de::Error::custom(format!(
                                "invalid regular expression '{s}': {e}"
                            )));
                        }
                        Ok(x) => x,
                    };
                    if is_parsing_prefer {
                        prefer.push(r);
                        if prefer.len() > MAX {
                            return Err(serde::de::Error::custom("too many extraction name resolution rules"));
                        }
                    } else {
                        dislike.push(r);
                        if dislike.len() > MAX {
                            return Err(serde::de::Error::custom("too many extraction name resolution rules"));
                        }
                    }
                }
                Ok(CfgExtractResolutionRules { prefer, dislike })
            }
        }
    }
}
