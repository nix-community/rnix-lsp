{ outputs ? [ "out" ], ... } @ drvAttrs:
let
  outputAttrs = builtins.listToAttrs (map
    (name: { inherit name; value = toValue name; })
    outputs);
  strict = derivationStrict drvAttrs;
  toValue = outputName: drvAttrs // outputAttrs // {
    inherit drvAttrs outputName;
    inherit (strict) drvPath;
    outPath = strict.${outputName};
    type = "derivation";
    all = map toValue outputs;
  };
in
toValue (builtins.head outputs)
