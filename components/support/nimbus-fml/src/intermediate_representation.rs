/* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/. */
use crate::error::{FMLError, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{collections::{HashMap, HashSet}, path};

/// The `TypeRef` enum defines a reference to a type.
///
/// Other types will be defined in terms of these enum values.
///
/// They represent the types available via the current `Variables` API—
/// some primitives and structural types— and can be represented by
/// Kotlin, Swift and JSON Schema.
///
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum TypeRef {
    // Current primitives.
    String,
    Int,
    Boolean,

    // Strings can be coerced into a few types.
    // The types here will require the app's bundle or context to look up the final value.
    // They will likely have
    BundleText(StringId),
    BundleImage(StringId),

    Enum(String),
    // JSON objects can represent a data class.
    Object(String),

    // JSON objects can also represent a `Map<String, V>` or a `Map` with
    // keys that can be derived from a string.
    StringMap(Box<TypeRef>),
    // We can coerce the String keys into Enums, so this represents that.
    EnumMap(Box<TypeRef>, Box<TypeRef>),

    List(Box<TypeRef>),
    Option(Box<TypeRef>),
}

pub(crate) type StringId = String;
pub(crate) type FMLPath = (Vec<String>, TypeRef);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FeatureManifest {
    pub enum_defs: Vec<EnumDef>,
    pub obj_defs: Vec<ObjectDef>,
    // `hints` are useful for things that will be constructed from strings
    // such as images and display text.
    pub hints: HashMap<StringId, FromStringDef>,
    pub feature_defs: Vec<FeatureDef>,
}

impl FeatureManifest {
    #[allow(dead_code)]
    fn validate_manifest(&self) -> Result<()> {
        // We first validate that each enum_def has a unique name.
        // TODO: We repeat this check three times, it should be its
        // own generic helper
        let mut enum_names = HashSet::new();
        self.validate_enum_defs(&mut enum_names)?;
        // We then validate that each obj_defs also has a unique name.
        let mut obj_names = HashSet::new();
        self.validate_obj_defs(&mut obj_names)?;

        // We then validate that each feature_def has a unique name.
        let mut feature_names = HashSet::new();
        self.validate_feature_defs(&mut feature_names)?;

        // We then validate that each type_ref is valid
        for feature_def in &self.feature_defs {
            for prop in &feature_def.props {
                self.validate_type_ref(&prop.typ, &enum_names, &obj_names)?;
            }
        }
        self.validate_defaults()?;
        Ok(())
    }

    fn validate_type_ref(
        &self,
        type_ref: &TypeRef,
        enum_names: &HashSet<String>,
        obj_names: &HashSet<String>,
    ) -> Result<()> {
        match type_ref {
            TypeRef::Enum(name) => {
                if !enum_names.contains(name) {
                    return Err(FMLError::ValidationError(format!(
                        "Found enum reference with name: {}, but no definition",
                        name
                    )));
                }
                Ok(())
            }
            TypeRef::Object(name) => {
                if !obj_names.contains(name) {
                    return Err(FMLError::ValidationError(format!(
                        "Found object reference with name: {}, but no definition",
                        name
                    )));
                }
                Ok(())
            }
            TypeRef::EnumMap(key_type, value_type) => {
                if let TypeRef::Enum(_) = key_type.as_ref() {
                    self.validate_type_ref(key_type, enum_names, obj_names)?;
                } else {
                    return Err(FMLError::ValidationError(format!(
                        "EnumMap key has be an enum, found: {:?}",
                        key_type
                    )));
                }
                self.validate_type_ref(value_type, enum_names, obj_names)
            }
            TypeRef::List(list_type) => self.validate_type_ref(list_type, enum_names, obj_names),
            TypeRef::StringMap(value_type) => {
                self.validate_type_ref(value_type, enum_names, obj_names)
            }
            TypeRef::Option(option_type) => {
                if let TypeRef::Option(_) = option_type.as_ref() {
                    return Err(FMLError::ValidationError(
                        "Found nested optional types".into(),
                    ));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn validate_enum_defs(&self, enum_names: &mut HashSet<String>) -> Result<()> {
        for enum_def in &self.enum_defs {
            if !enum_names.insert(enum_def.name.clone()) {
                return Err(FMLError::ValidationError(format!(
                    "EnumDef names must be unique. Found two EnumDefs with the same name: {}",
                    enum_def.name
                )));
            }
        }
        Ok(())
    }

    fn validate_obj_defs(&self, obj_names: &mut HashSet<String>) -> Result<()> {
        for obj_def in &self.obj_defs {
            if !obj_names.insert(obj_def.name.clone()) {
                return Err(FMLError::ValidationError(format!(
                    "ObjectDef names must be unique. Found two ObjectDefs with the same name: {}",
                    obj_def.name
                )));
            }
        }
        Ok(())
    }

    fn validate_feature_defs(&self, feature_names: &mut HashSet<String>) -> Result<()> {
        for feature_def in &self.feature_defs {
            if !feature_names.insert(feature_def.name.clone()) {
                return Err(FMLError::ValidationError(format!(
                    "FeatureDef names must be unique. Found two FeatureDefs with the same name: {}",
                    feature_def.name
                )));
            }
            // while checking the feature, we also check that each prop is unique within a feature
            let mut prop_names = HashSet::new();
            for prop in &feature_def.props {
                if !prop_names.insert(prop.name.clone()) {
                    return Err(FMLError::ValidationError(format!(
                        "PropDef names must be unique. Found two PropDefs with the same name: {} in the same feature_def: {}",
                        prop.name, feature_def.name
                    )));
                }
            }
        }
        Ok(())
    }


    // Validates that each property has a defined default
    // for now, this doesn't look at the definitions themselves
    // and only looks at the feature_definitions
    // Step 1: Iterate over all features
    //  Step 2: for each feature, we first collect all the "paths" to each
    //          primitive value, if it's a top level property, the path is an
    //          empty vector, otherwise, it's an ordered vector with the names
    //          of each property on the path to the primitive value
    //  Step 3: We validate against the default defined for the current property
    //          if the all the paths we collected on Step 2, have a default
    //          we end here, otherwise
    //  Step 4: We seperate the paths remaining that have the same first element
    //          and recurse into the property of that name. The recursive
    //          function should return the path to the property that doesn't have
    //          a default, so the high level error handling, can report it to the
    //          user
    fn validate_defaults(&self) -> Result<()> {
        // TODO: Validate that the defaults of each feature
        // in the feature manifest agree with the props
        // There must be a default for each property
        // either that's defined in the property itself
        // or in the feature definition
        // Step 1: Iterate over all the features
        for feature_def in &self.feature_defs {
            // Step 2: Gather a list of all the paths to primitives
            let paths_to_primitives = self.get_paths_to_primitives(feature_def)?;
            let missing_paths = self.validate_paths_against_defaults(feature_def, &paths_to_primitives)?;
            if !missing_paths.is_empty() {
                return Err(FMLError::ValidationError(format!(
                    "Missing default for paths: {:?}",
                    missing_paths
                )));
            }
        }
        Ok(())
    }

    fn get_paths_to_primitives(&self, feature_def: &FeatureDef) -> Result<Vec<FMLPath>> {
        let mut paths = Vec::new();
        let mut curr_path = Vec::new();
        for prop in &feature_def.props {
            self.get_paths_to_primitives_prop(prop, &mut curr_path, &mut paths)?;
        }

        Ok(paths)
    }

    fn get_paths_to_primitives_prop(
        &self,
        prop: &PropDef,
        curr_path: &mut Vec<String>,
        paths: &mut Vec<FMLPath>,
    ) -> Result<()> {
        curr_path.push(prop.name.clone());
        // We check if the property is a primitive
        self.get_paths_to_primitives_typ(&prop.typ, curr_path, paths)?;
        curr_path.pop();
        Ok(())
    }

    fn get_paths_to_primitives_typ(
        &self,
        typ: &TypeRef,
        curr_path: &mut Vec<String>,
        paths: &mut Vec<(Vec<String>,TypeRef)>,
    ) -> Result<()> {
        match typ {
            TypeRef::Object(name) => {
                if let Some(obj_def) = self.obj_defs.iter().find(|o| o.name == *name) {
                    for other_prop in &obj_def.props {
                        self.get_paths_to_primitives_prop(other_prop, curr_path, paths)?;
                    }
                } else {
                    return Err(FMLError::ValidationError(format!(
                        "Object referenced but not defined: {}",
                        name
                    )));
                }
            }
            TypeRef::EnumMap(key, value) => {
                if let TypeRef::Enum(enum_def_name) = key.as_ref() {
                    if let Some(enum_def) = self.enum_defs.iter().find(|o| o.name == *enum_def_name) {
                        for variant in &enum_def.variants {
                            curr_path.push(variant.name.clone());
                            self.get_paths_to_primitives_typ(value, curr_path, paths)?;
                            curr_path.pop();
                        }
                    } else {
                        return Err(FMLError::ValidationError(format!(
                            "Enum referenced but not defined: {}",
                            enum_def_name
                        )));
                    }
                } else {
                    return Err(FMLError::ValidationError(format!(
                        "EnumMap key is not an enum: {:?}",
                        key
                    )));
                }
            },
            // Since the keys can be arbitrary strings,
            // we cannot validate that the defaults of
            // StringMaps match the property definition,
            // instead, we terminate here, and when we later
            // match against the defaults, as long as there
            // exists a string map, we count it as OK
            _ => {
                paths.push((curr_path.clone(), typ.clone()))
            }
        }
        Ok(())
    }

    fn validate_paths_against_defaults(
        &self,
        feature_def: &FeatureDef,
        paths_to_primitives: &Vec<FMLPath>,
    ) -> Result<Vec<FMLPath>> {
        let mut missing_paths = paths_to_primitives.clone();

        if let Some(default) = &feature_def.default {
            missing_paths = self.validate_paths_against_default(&missing_paths, default)?;
            if missing_paths.is_empty() {
                return Ok(missing_paths)
            }
        }
        let mut ret = Vec::new();
        for prop in &feature_def.props {
            // We filter a collection that starts with this prop's name
            let missing_paths_for_prop = missing_paths.iter().filter(|path| !path.0.is_empty() && path.0[0] == prop.name)
            .map(|path| {
                // Unwrap is safe, we check that the vector is not empty on the filter
                let path_without_first = path.0.split_first().unwrap().1.to_vec();
                (path_without_first, path.1.clone())
            }).collect::<Vec<FMLPath>>();
            ret.append(&mut self.validate_paths_against_prop_defaults(prop, &missing_paths_for_prop)?);
        }
        Ok(ret)
    }

    fn validate_paths_against_prop_defaults(&self, prop: &PropDef, paths_to_primitives: &Vec<FMLPath>) -> Result<Vec<FMLPath>> {
        let missing_paths = self.validate_paths_against_default(&paths_to_primitives, &prop.default)?;
        if missing_paths.is_empty() {
            return Ok(missing_paths);
        }

        


        Ok(missing_paths)
    }

    fn validate_paths_against_default(&self, paths_to_primitives: &Vec<FMLPath>, default: &Value) -> Result<Vec<FMLPath>> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FeatureDef {
    name: String,
    doc: String,
    props: Vec<PropDef>,
    default: Option<Literal>,
}
impl FeatureDef {
    #[allow(dead_code)]
    pub fn new(name: &str, doc: &str, props: Vec<PropDef>, default: Option<Literal>) -> Self {
        Self {
            name: name.into(),
            doc: doc.into(),
            props,
            default,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub doc: String,
    pub variants: Vec<VariantDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FromStringDef {
    pub name: String,
    pub doc: String,
    pub variants: Vec<VariantDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct VariantDef {
    name: String,
    doc: String,
}
impl VariantDef {
    #[allow(dead_code)]
    pub fn new(name: &str, doc: &str) -> Self {
        Self {
            name: name.into(),
            doc: doc.into(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ObjectDef {
    name: String,
    doc: String,
    props: Vec<PropDef>,
}
impl ObjectDef {
    #[allow(dead_code)]
    pub fn new(name: &str, doc: &str, props: Vec<PropDef>) -> Self {
        Self {
            name: name.into(),
            doc: doc.into(),
            props,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PropDef {
    pub name: String,
    pub doc: String,
    pub typ: TypeRef,
    pub default: Literal,
}

type Literal = Value;

#[cfg(test)]
mod unit_tests {
    use std::vec;

    use serde_json::json;

    use super::*;
    use crate::error::Result;
    use crate::fixtures::intermediate_representation::{self, get_simple_homescreen_feature};

    #[test]
    fn can_ir_represent_smoke_test() -> Result<()> {
        let m1 = intermediate_representation::get_simple_homescreen_feature();
        let string = serde_json::to_string(&m1)?;
        let m2: FeatureManifest = serde_json::from_str(&string)?;

        assert_eq!(m1, m2);

        Ok(())
    }

    #[test]
    fn validate_good_feature_manifest() -> Result<()> {
        let fm = get_simple_homescreen_feature();
        fm.validate_manifest()
    }

    #[test]
    fn validate_duplicate_enum_defs_fail() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.enum_defs.push(EnumDef {
            name: "SectionId".into(),
            doc: "The sections of the homescreen".into(),
            variants: vec![
                VariantDef::new("top-sites", "The original frecency sorted sites"),
                VariantDef::new("jump-back-in", "Jump back in section"),
                VariantDef::new("recently-saved", "Tabs that have been bookmarked recently"),
            ],
        });
        fm.validate_manifest()
            .expect_err("Should fail on duplicate enum_defs");
        Ok(())
    }

    #[test]
    fn validate_duplicate_obj_defs_fails() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.obj_defs = vec![
            ObjectDef {
                name: "SimpleObjDef".into(),
                doc: "Simpel doc".into(),
                props: vec![],
            },
            ObjectDef {
                name: "SimpleObjDef".into(),
                doc: "Simpel doc".into(),
                props: vec![],
            },
        ];
        fm.validate_manifest()
            .expect_err("Should fail on duplicate obj_defs");
        Ok(())
    }

    #[test]
    fn validate_duplicate_feature_defs_fails() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "homescreen",
            "Represents the homescreen feature",
            vec![PropDef {
                name: "sections-enabled".into(),
                doc: "A map of booleans".into(),
                typ: TypeRef::EnumMap(
                    Box::new(TypeRef::Enum("SectionId".into())),
                    Box::new(TypeRef::String),
                ),
                default: json!({
                    "top-sites": true,
                    "jump-back-in": false,
                    "recently-saved": false,
                }),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail on duplicate feature defs");
        Ok(())
    }

    #[test]
    fn validate_duplicate_props_in_same_feature_fails() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "otherhomescreen",
            "Represents the homescreen feature",
            vec![
                PropDef {
                    name: "duplicate-prop".into(),
                    doc: "A map of booleans".into(),
                    typ: TypeRef::EnumMap(
                        Box::new(TypeRef::Enum("SectionId".into())),
                        Box::new(TypeRef::String),
                    ),
                    default: json!({
                        "top-sites": true,
                        "jump-back-in": false,
                        "recently-saved": false,
                    }),
                },
                PropDef {
                    name: "duplicate-prop".into(),
                    doc: "A map of booleans".into(),
                    typ: TypeRef::EnumMap(
                        Box::new(TypeRef::Enum("SectionId".into())),
                        Box::new(TypeRef::String),
                    ),
                    default: json!({
                        "top-sites": true,
                        "jump-back-in": false,
                        "recently-saved": false,
                    }),
                },
            ],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail on duplicate props in the same feature");
        Ok(())
    }

    #[test]
    fn validate_enum_type_ref_doesnt_match_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::Enum("EnumDoesntExist".into()),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest().expect_err(
            "Should fail since EnumDoesntExist isn't a an enum defined in the manifest",
        );
        Ok(())
    }

    #[test]
    fn validate_obj_type_ref_doesnt_match_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::Object("ObjDoesntExist".into()),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest().expect_err(
            "Should fail since ObjDoesntExist isn't a an Object defined in the manifest",
        );
        Ok(())
    }

    #[test]
    fn validate_enum_map_with_non_enum_key() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::EnumMap(Box::new(TypeRef::String), Box::new(TypeRef::String)),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail since the key on an EnumMap must be an Enum");
        Ok(())
    }

    #[test]
    fn validate_list_with_enum_with_no_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::List(Box::new(TypeRef::Enum("EnumDoesntExist".into()))),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail EnumDoesntExist isn't a an enum defined in the manifest");
        Ok(())
    }

    #[test]
    fn validate_enum_map_with_enum_with_no_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::EnumMap(
                    Box::new(TypeRef::Enum("EnumDoesntExist".into())),
                    Box::new(TypeRef::String),
                ),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest().expect_err(
            "Should fail since EnumDoesntExist isn't a an enum defined in the manifest",
        );
        Ok(())
    }

    #[test]
    fn validate_enum_map_with_obj_value_no_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::EnumMap(
                    Box::new(TypeRef::Enum("SectionId".into())),
                    Box::new(TypeRef::Object("ObjDoesntExist".into())),
                ),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail since ObjDoesntExist isn't an Object defined in the manifest");
        Ok(())
    }

    #[test]
    fn validate_string_map_with_enum_value_no_def() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::StringMap(Box::new(TypeRef::Enum("EnumDoesntExist".into()))),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail since ObjDoesntExist isn't an Object defined in the manifest");
        Ok(())
    }

    #[test]
    fn validate_nested_optionals_fail() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        fm.feature_defs.push(FeatureDef::new(
            "some_def",
            "test doc",
            vec![PropDef {
                name: "prop name".into(),
                doc: "prop doc".into(),
                typ: TypeRef::Option(Box::new(TypeRef::Option(Box::new(TypeRef::String)))),
                default: json!(null),
            }],
            None,
        ));
        fm.validate_manifest()
            .expect_err("Should fail since we can't have nested optionals");
        Ok(())
    }
    // TODO: Add more tests for defaults

    #[test]
    fn test_get_paths_to_primitives() -> Result<()> {
        let mut fm = get_simple_homescreen_feature();
        // we add more paths to stress test it
        fm.feature_defs[0].props.push(
            PropDef {
                name: "other-prop".into(),
                typ: TypeRef::Object("TestObject".into()),
                default: json!(null),
                doc: "test doc".into(),
            }
        );

        fm.obj_defs.push(
            ObjectDef {
                name: "TestObject".into(),
                props: vec![
                    PropDef {
                        name: "an-int".into(),
                        typ: TypeRef::Int,
                        doc: "".into(),
                        default: json!(null),
                    },
                    PropDef {
                        name: "a-list".into(),
                        typ: TypeRef::List(Box::new(TypeRef::Int)),
                        doc: "".into(),
                        default: json!(null),
                    },
                    PropDef {
                        name: "an-enum".into(),
                        typ: TypeRef::Enum("TestEnum".into()),
                        doc: "".into(),
                        default: json!(null),
                    },
                    PropDef {
                        name: "nested-enum-map".into(),
                        typ: TypeRef::EnumMap(
                            Box::new(TypeRef::Enum("SectionId".into())),
                            Box::new(TypeRef::Boolean),
                        ),
                        default: json!(null),
                        doc: "Test-enum-map".into()
                    }
                ],
                doc: "Test doc".into(),
            }
        );

        let paths = fm.get_paths_to_primitives(&fm.feature_defs[0])?;
        assert_eq!(paths, vec![
            (vec!["sections-enabled".into(), "top-sites".into()], TypeRef::Boolean),
            (vec!["sections-enabled".into(), "jump-back-in".into()], TypeRef::Boolean),
            (vec!["sections-enabled".into(), "recently-saved".into()], TypeRef::Boolean),
            (vec!["other-prop".into(), "an-int".into()], TypeRef::Int),
            (vec!["other-prop".into(), "a-list".into()], TypeRef::List(Box::new(TypeRef::Int))),
            (vec!["other-prop".into(), "an-enum".into()], TypeRef::Enum("TestEnum".into())),
            (vec!["other-prop".into(), "nested-enum-map".into(), "top-sites".into()], TypeRef::Boolean),
            (vec!["other-prop".into(), "nested-enum-map".into(), "jump-back-in".into()], TypeRef::Boolean),
            (vec!["other-prop".into(), "nested-enum-map".into(), "recently-saved".into()], TypeRef::Boolean),
        ]);
        Ok(())
    }
}
