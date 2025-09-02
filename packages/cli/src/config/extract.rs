use cu::pre::*;
use tyyaml::Prim;

use crate::serde_impl::Regex;

/// Config for extract
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgExtract {
    /// Pointer width for the target platform, must be 8, 16, 32 or 64
    pub pointer_width: u8,
    /// Representation of PTMD, as an array of primitive
    pub ptmd_repr: (Prim, u32),
    /// Representation of PTMF, as an array of primitive
    pub ptmf_repr: (Prim, u32),
    /// Regex for the virtual function pointer field
    pub vfptr_field_regex: Regex,
    /// Rules for the type parser
    pub type_parser: CfgExtractTypeParser,
    /// Rules for resolving type names
    pub name_resolution: CfgExtractResolution,
}

impl CfgExtract {
    /// Get the primitive equivalent of a pointer type
    pub fn pointer_type(&self) -> cu::Result<Prim> {
        let pointer_type = match self.pointer_width {
            8 => Prim::U8,
            16 => Prim::U16,
            32 => Prim::U32,
            64 => Prim::U64,
            x => cu::bail!("invalid pointer width in config: {x}"),
        };
        Ok(pointer_type)
    }

    /// Get the byte size of the pointer
    pub fn pointer_size(&self) -> cu::Result<u32> {
        let size = match self.pointer_width {
            8 => 1,
            16 => 2,
            32 => 4,
            64 => 8,
            x => cu::bail!("invalid pointer width in config: {x}"),
        };
        Ok(size)
    }

    pub fn ptmd_size(&self) -> cu::Result<u32> {
        let mut size = cu::check!(self.ptmd_repr.0.byte_size(), "invalid unsized ptmd repr in config")?;
        size *= self.ptmd_repr.1;
        cu::ensure!(size != 0, "invalid zero-sized ptmd repr in config");
        Ok(size)
    }

    pub fn ptmf_size(&self) -> cu::Result<u32> {
        let mut size = cu::check!(self.ptmf_repr.0.byte_size(), "invalid unsized ptmf repr in config")?;
        size *= self.ptmf_repr.1;
        cu::ensure!(size != 0, "invalid zero-sized ptmf repr in config");
        Ok(size)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgExtractTypeParser {
    /// If the fully-qualified typedef name matches these regexes,
    /// the typedefed name will be abandoned, and the inner type (typedef target)
    /// will be used instead of the typedef
    #[serde(default)]
    pub abandon_typedefs: Vec<Regex>,
}

/// Config for name resolution for the extract command
#[derive(Debug, Clone, Deserialize)]
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

#[derive(Debug, Clone)]
pub struct CfgExtractResolutionRules {
    /// Pattern for preferrence, from more preferred to less preferred
    pub prefer: Vec<regex::Regex>,
    /// Pattern for dislikeness, from less disliked to more disliked
    pub dislike: Vec<regex::Regex>,
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
                    let r = match regex::Regex::new(s) {
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
