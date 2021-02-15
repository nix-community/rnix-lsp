let
  # This data can be used for checking tab completions
  inline_set = {
    shared_prefix_for_first_item = 1;
    shared_prefix_for_second_item = 2;
    shared_prefix_for_third_item = 3;
    nested_completions = {
      also_shared_prefix_for_first_item = 4;
      also_shared_prefix_for_second_item = 5;
      also_shared_prefix_for_third_item = 6;
    };
    nested_completions_alternative = {
      another_shared_prefix_for_first_item = 7;
      another_shared_prefix_for_second_item = 8;
      another_shared_prefix_for_third_item = 9;
    };
  };
  imported_set = import ./imported-set.nix;

  # This can be used for checking renames
  rename_me = "the original";

  used_in_set = {
    this_is_an_interpolation = "${rename_me}";
    this_is_a_value = rename_me;
    used_in_nested_set = {
      this_is_also_an_interpolation = "${rename_me}";
      this_is_also_a_value = rename_me;
    };

    # Ignoring shadowed variables is currently broken
    shadowed = rec {
      rename_me = "unrelated, so if possible, don't *actually* rename me";
      hopefully_unchanged_usage = rename_me;
    };
  };
in
[
  inline_set.shared_prefix_for_first_item
]
