import { TmLanguage } from "./tmlanguage";

/** Convert refs into an array of patterns that only include the given ref.
 * This is a convience function for simplifying
 *
 * ```ts
 * // ...
 *     patterns: [ { include: "..." }, { include: "..." }, { include: "..." } ]
 * // ...
 * ```
 *
 * @param refs references to patterns in the repository, or scopes
 * @returns
 */
const include = (...refs: string[]) => refs.map((r) => ({ include: r }));

const language: TmLanguage = {
  name: "Zustral",
  patterns: [
    {
      match: "\\.",
      name: "punctuation.delimiter.module.end.zustral",
    },
    ...include(
      "#import",
      "#import_simple",
      "#module_body",
      "#module",
      "#context_block"
    ),
  ],
  repository: {
    keywords: {
      patterns: [
        {
          name: "keyword.control.$0.zustral",
          match:
            "\\b(skip|import|if|then|else|case|when|whiledo|of|for|return|(else\\s+if))\\b",
        },
        {
          name: "keyword.control.end.$1.zustral",
          match: "\\bend\\s+(if|case|while|for)\\b",
        },
        {
          name: "storage.type.$0.zustral",
          match:
            "\\b(constant|let|generic|function|method|instance|record|union|type|typeclass)\\b",
        },
        {
          name: "keyword.other.module.interface.zustral storage.type.module.interface.zustral",
          match: "\\bmodule\\b",
        },
        {
          name: "keyword.other.module.body.zustral storage.type.module.body.zustral",
          match: "\\bmodule\\s+body\\b",
        },
        {
          name: "keyword.other.end.module.interface.zustral storage.type.module.interface.zustral",
          match: "\\bend\\s+module\\b",
        },
        {
          name: "keyword.other.end.module.body.zustral storage.type.module.body.zustral",
          match: "\\bend\\s+module\\s+body\\b",
        },
        {
          match: "\\bsizeof\\b",
          name: "keyword.operator.sizeof.zustral keyword.other.sizeof.zustral",
        },
        {
          match: "(\\@embed)",
          name: "keyword.operator.embed.zustral keyword.other.embed.zustral",
        },
        {
          name: "keyword.other.$0.zustral",
          match: "\\b(pragma|is|end)\\b",
        },
        {
          name: "keyword.other.borrow.zustral",
          match: "\\bborrow\\!?\\b",
        },
      ],
    },
    comment: {
      match: "\\s*(--).*",
      name: "comment.line.zustral",
      captures: {
        "1": {
          name: "punctuation.definition.comment.zustral",
        },
      },
    },
    context_import_symbol_list: {
      name: "meta.import.symbols.zustral",
      patterns: [
        {
          include: "#comment",
        },
        {
          match: "\\bas\\b",
          name: "keyword.control.import.as.zustral",
        },
        {
          name: "punctuation.separator.delimiter.comma.austal",
          match: ",",
        },
        {
          match: "\\b([a-zA-Z][a-zA-Z0-9_]*)\\b",
          name: "entity.name.import-symbol.zustral",
        },
      ],
    },
    context_paramater_list: {
      comment: "paramater lists for pragmas and method/function calls",
      patterns: [
        {
          comment: "named paramater assignment",
          match: "\\b([a-zA-Z][a-zA-Z0-9_]*)\\s*(=>)",
          captures: {
            "1": {
              name: "variable.parameter.function-call.zustral",
            },
            "2": {
              name: "keyword.operator.assignment.named-paramater.zustral",
            },
          },
        },
        {
          include: "#context_expression",
        },
        {
          name: "punctuation.separator.delimiter.comma.austal",
          match: ",",
        },
      ],
    },
    context_expression: {
      name: "meta.expression.zustral",
      patterns: [
        {
          include: "#comment",
        },
        {
          include: "#constants",
        },
        {
          include: "#operators",
        },
        {
          match: "(:)\\s*([a-zA-Z][a-zA-Z0-9_]*(?:\\[.*\\])?)",
          captures: {
            "1": {
              name: "keyword.operator.typecast.zustral",
            },
            "2": {
              patterns: [
                {
                  include: "#typespec",
                },
              ],
            },
          },
        },
        {
          comment: "sub-expression",
          begin: "\\(",
          end: "\\)",
          beginCaptures: {
            "0": {
              name: "punctuation.section.expression.begin.bracket.round.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.section.expression.begin.bracket.round.zustral",
            },
          },
          patterns: [
            {
              include: "#context_expression",
            },
          ],
        },
        {
          comment: "sizeof operator",
          begin: "\\b(sizeof)\\s*(\\()",
          end: "\\)",
          beginCaptures: {
            "1": {
              name: "keyword.operator.sizeof.zustral",
            },
            "2": {
              name: "punctuation.section.arguments.begin.bracket.round.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.section.arguments.begin.bracket.round.zustral",
            },
          },
          patterns: [
            {
              include: "#typespec",
            },
          ],
        },
        {
          include: "#function_call",
        },
        {
          match: "\\b([a-zA-Z][a-zA-Z0-9_]*)\\b",
          name: "variable.other.zustral",
        },
        {
          include: "#path",
        },
      ],
    },
    context_type_parameter_list: {
      name: "meta.type-paramater-list.zustral",
      patterns: [
        {
          include: "#region_assertion",
        },
        {
          match: "([a-zA-Z][a-zA-Z0-9_]*)",
          name: "entity.name.type.zustral",
        },
        {
          include: "#comment",
        },
        {
          match: ",",
          name: "punctuation.comma.zustral",
        },
        {
          match: "[\\(\\)]",
          name: "punctuation.brackets.round.zustral",
        },
      ],
    },
    context_function_paramater_def: {
      name: "meta.function-decl.parameters.zustral",
      patterns: [
        {
          include: "#comment",
        },
        {
          match: "([a-zA-Z][a-zA-Z0-9_]*)\\s*(:)",
          captures: {
            "1": {
              name: "variable.parameter.zustral",
            },
            "2": {
              name: "punctuation.colon.zustral",
            },
          },
        },
        {
          include: "#typespec",
        },
        {
          name: "punctuation.brackets.round.zustral",
          match: "[\\(\\)]",
        },
      ],
    },
    context_binding_list: {
      name: "meta.bindings.zustral",
      patterns: [
        {
          include: "#comment",
        },
        {
          match: "(:)\\s*([a-zA-Z][a-zA-Z0-9_]*(?:\\[(.*)\\])?)",
          captures: {
            "1": {
              name: "keyword.operator.type.annotation.zustral",
            },
            "2": {
              name: "meta.binding-type.zustral",
              patterns: [
                {
                  include: "#typespec",
                },
              ],
            },
          },
        },
        {
          match: "\\b[a-zA-Z][a-zA-Z0-9_]*\\b",
          name: "variable.other.binding.zustral",
        },
        {
          match: "\\bas\\b",
          name: "keyword.operator.as.zustral",
        },
      ],
    },
    context_slot_list: {
      patterns: [
        {
          include: "#comment",
        },
        {
          include: "#string_triple",
        },
        {
          match: ";",
          name: "punctuation.delimiter.slot.end.zustral",
        },
        {
          include: "#slot",
        },
      ],
    },
    context_block: {
      patterns: [
        {
          match: ";",
          name: "punctuation.terminator.statement.zustral",
        },
        ...include(
          "#comment",
          "#let_simple",
          "#pragma",
          "#let_destructure",
          "#borrow_head",
          "#if_head",
          "#case_head",
          "#for_loop_head",
          "#while_loop_head",
          "#if_body",
          "#case_body",
          "#do_block",
          "#constant_decl",
          "#keywords",
          "#context_expression",
          "#assignment"
        ),
      ],
    },
    universe_assertion: {
      comment:
        "colon followed by a universe, used for ascribing a universe to a type",
      match: "(:)\\s+(Free|Linear|Type|Region)",
      captures: {
        "1": {
          name: "punctuation.delimiter.region.colon.zustral",
        },
        "2": {
          name: "entity.name.universe.zustral entity.name.type.zustral",
        },
      },
    },
    context_module_interface: {
      patterns: [
        ...include(
          "#comment",
          "#string_triple",
          "#pragma",
          "#constant_decl",
          "#record_body",
          "#union_body",
          "#union_or_record_head"
        ),
        {
          comment: "type declaration",
          name: "meta.type.zustral",
          match:
            "(type)\\s+([a-zA-Z][a-zA-Z0-9_]*)\\s*(?:(\\[)(.*)(\\]))?\\s*(?:(:)\\s*(Free|Linear|Type|Region))?",
          captures: {
            "1": {
              name: "keyword.declaration.type.zustral storage.type.zustral",
            },
            "2": {
              name: "entity.name.type.zustral",
            },
            "3": {
              name: "punctuation.brackets.square.zustral",
            },
            "4": {
              patterns: [
                {
                  include: "#context_type_parameter_list",
                },
              ],
            },
            "5": {
              name: "punctuation.brackets.square.zustral",
            },
            "6": {
              name: "punctuation.colon.zustral",
            },
            "7": {
              name: "entity.name.universe.zustral entity.name.type.zustral",
            },
          },
        },
        ...include(
          "#generic",
          "#function_head",
          "#typeclass_head",
          "#typeclass_body",
          "#instance_head",
          "#keywords"
        ),
      ],
    },
    context_module_body: {
      patterns: include(
        "#instance_body",
        "#function_body",
        "#assignment",
        "#context_module_interface"
      ),
    },
    import_simple: {
      comment: "initial highlighting on import without the symbol list",
      name: "meta.import.zustral",
      match:
        "\\b(import)\\s+((?:[a-zA-Z][a-zA-Z0-9_]*)(?:\\.[a-zA-Z][a-zA-Z0-9_]*)*)",
      captures: {
        "1": {
          name: "keyword.control.import.zustral",
        },
        "2": {
          name: "entity.name.module.zustral",
        },
      },
    },
    import: {
      name: "meta.import.zustral",
      begin:
        "\\b(import)\\s+((?:[a-zA-Z][a-zA-Z0-9_]*)(?:\\.[a-zA-Z][a-zA-Z0-9_]*)*)\\s*(\\()",
      end: "\\)",
      beginCaptures: {
        "1": {
          name: "keyword.control.import.zustral",
        },
        "2": {
          name: "entity.name.module.zustral",
        },
        "3": {
          name: "punctuation.section.imports.begin.bracket.round.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.section.imports.begin.bracket.round.zustral",
        },
      },
      patterns: [
        {
          include: "#context_import_symbol_list",
        },
      ],
    },
    generic: {
      name: "meta.generic-decl.zustral",
      begin: "(generic)\\s*(\\[)",
      end: "\\]",
      beginCaptures: {
        "1": {
          name: "keyword.other.generic.zustral",
        },
        "2": {
          name: "punctuation.brackets.square.zustral",
        },
      },
      patterns: [
        {
          include: "#context_type_parameter_list",
        },
      ],
    },
    constants: {
      patterns: include(
        "#float_constant",
        "#bool_constants",
        "#nil_constant",
        "#integer_constants",
        "#triple_string",
        "#string"
      ),
    },
    assignment: {
      comment: "the := ...; part of an assignment statement",
      begin: ":=",
      end: ";",
      beginCaptures: {
        "0": {
          name: "keyword.operator.assign.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.semicolon.zustral",
        },
      },
      patterns: [
        {
          include: "#context_expression",
        },
      ],
    },
    float_constant: {
      comment: "decimal float constant",
      match:
        "([+\\-])?[0-9][0-9']*\\.([0-9][0-9']*)?([eE][+\\-]?[0-9][0-9']*)?",
      name: "constant.numeric.decimal.float.zustral",
    },
    bool_constants: {
      comment: "boolean constant",
      match: "\\b(true|false)\\b",
      name: "constant.language.bool.zustral",
    },
    nil_constant: {
      comment: "nil constant",
      match: "\\bnil\\b",
      name: "constant.language.nil.zustral",
    },
    integer_constants: {
      patterns: [
        {
          comment: "decimal integer constant",
          match: "([+\\-])?[0-9][0-9']*",
          captures: {
            "1": {
              name: "keyword.operator.sign.zustral",
            },
          },
          name: "constant.numeric.decimal.zustral",
        },
        {
          comment: "hexadecimal integer constant",
          match: "#x[0-9a-fA-F][0-9a-fA-F']*",
          name: "constant.numeric.hex.zustral",
        },
        {
          comment: "octal integer constant",
          match: "#o[0-7][0-7']*",
          name: "constant.numeric.octal.zustral",
        },
        {
          comment: "binary integer constant",
          match: "#b[0-1][0-1']*",
          name: "constant.numeric.bin.zustral",
        },
        {
          comment: "char (Nat8) constant",
          name: "string.quoted.single.char.zustral",
          begin: "'",
          end: "'",
          beginCaptures: {
            "0": {
              name: "punctuation.definition.char.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.definition.char.zustral",
            },
          },
          patterns: [
            {
              include: "#escapes",
            },
            {
              match: ".{2,}(?=')",
              name: "invalid.illegal.char-literal.zustral",
            },
            {
              comment: "Valid ASCII chars for char constants",
              match:
                "[a-zA-Z0-9\\ \\!\"#\\$%&'\\)\\(*+,\\-\\./:;<=>?@`~\\[\\]^_{}|]",
            },
            {
              match: ".*(?=')",
              name: "invalid.illegal.char-literal.zustral",
            },
          ],
        },
      ],
    },
    path: {
      patterns: [
        {
          comment: "array access",
          name: "meta.access.array.zustral",
          begin: "(?<=[\\)\\]a-zA-Z0-9_])(\\[)",
          end: "\\]",
          beginCaptures: {
            "1": {
              name: "punctuation.definition.begin.bracket.square.c",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.definition.end.bracket.square.c",
            },
          },
          patterns: include("#context_expression"),
        },
        {
          comment: "slot access (including pointer)",
          name: "meta.access.slot.zustral",
          match: "(?<=[\\)\\]a-zA-Z0-9_])(?:(\\.)|(->))([a-zA-Z][a-zA-Z0-9_]*)",
          captures: {
            "1": {
              name: "punctuation.separator.slot-access.zustral",
            },
            "2": {
              name: "punctuation.separator.pointer-slot-access.zustral",
            },
            "3": {
              name: "variable.other.member.zustral",
            },
          },
        },
      ],
    },
    function_call: {
      name: "meta.function-call.zustral",
      begin: "(?:(@embed)|([a-zA-Z][a-zA-Z0-9_]*))\\s*(\\()",
      end: "\\)",
      beginCaptures: {
        "1": {
          name: "keyword.operator.embed.zustral keyword.other.embed.zustral",
        },
        "2": {
          name: "entity.name.function.zustral",
        },
        "3": {
          name: "punctuation.section.arguments.begin.bracket.round.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.section.arguments.begin.bracket.round.zustral",
        },
      },
      patterns: [
        {
          include: "#context_paramater_list",
        },
      ],
    },
    pragma_params: {
      name: "meta.pragma.zustral",
      begin: "(pragma)\\s+([a-zA-Z][a-zA-Z0-9_]*)\\s*(\\()",
      end: "\\)",
      beginCaptures: {
        "1": {
          name: "keyword.other.pragma.zustral",
        },
        "2": {
          name: "entity.name.pragma.zustral",
        },
        "3": {
          name: "punctuation.section.arguments.begin.bracket.round.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.section.arguments.begin.bracket.round.zustral",
        },
      },
      patterns: [
        {
          name: "meta.pragma.arguments.zustral",
          include: "#context_paramater_list",
        },
      ],
    },
    pragma_simple: {
      match: "(pragma)\\s+([a-zA-Z][a-zA-Z0-9_]*)",
      captures: {
        "1": {
          name: "keyword.other.pragma.zustral",
        },
        "2": {
          name: "entity.name.pragma.zustral",
        },
      },
    },
    pragma: {
      patterns: include("#pragma_simple", "#pragma_params"),
    },
    operators: {
      patterns: [
        {
          comment: "logical operators",
          match: "\\b(not|and|or)\\b",
          name: "keyword.operator.logical.$0.zustral",
        },
        {
          comment: "comparison operators",
          match: "(=|/=|<=|<|>=|>)",
          name: "keyword.operator.comparison.zustral",
        },
        {
          comment: "borrow operators",
          match: "(&|&!)",
          name: "keyword.operator.borrow.zustral",
        },
        {
          comment: "arithmetic operators",
          match: "(\\+|-|/|\\*)",
          name: "keyword.operator.arithmetic.zustral",
        },
      ],
    },
    typespec: {
      name: "meta.typespec.zustral",
      patterns: [
        {
          comment: "paramterized types",
          begin: "([a-zA-Z][A-Za-z0-9_]*)(\\[)",
          beginCaptures: {
            "1": {
              name: "entity.name.type.zustral",
            },
            "2": {
              name: "punctuation.brackets.square.zustral",
            },
          },
          end: "]",
          endCaptures: {
            "0": {
              name: "punctuation.brackets.square.zustral",
            },
          },
          patterns: [
            {
              include: "#typespec",
            },
            {
              include: "#comment",
            },
            {
              match: ",",
              name: "punctuation.comma.zustral",
            },
          ],
        },
        {
          comment: "reference types",
          begin: "(&!)\\s*(\\[)",
          beginCaptures: {
            "1": {
              name: "operator.punctuation.borrow-write.zustral",
            },
            "2": {
              name: "punctuation.brackets.square.zustral",
            },
          },
          end: "]",
          endCaptures: {
            "0": {
              name: "punctuation.brackets.angle.zustral",
            },
          },
          patterns: [
            {
              include: "#context_type_parameter_list",
            },
          ],
        },
        {
          comment: "primitive types",
          name: "entity.name.type.primitive.zustral",
          match: "\\b(Bool|((Nat|Int)(8|16|32|64))|Index)\\b",
        },
        {
          name: "entity.name.type.zustral",
          match: "\\b[a-zA-Z][A-Za-z0-9_]*\\b",
        },
      ],
    },
    slot: {
      comment: "member values used in records and unions",
      name: "meta.slot.zustral",
      match:
        "([a-zA-Z][a-zA-Z0-9_]*)\\s*(?:(:)\\s*([a-zA-Z][a-zA-Z0-9_]*(?:\\[(.*)\\])?))?",
      captures: {
        "1": {
          name: "entity.name.member.zustral",
        },
        "2": {
          name: "keyword.operator.type.annotation.zustral",
        },
        "3": {
          name: "meta.slot.type.zustral",
          patterns: include("#typespec"),
        },
      },
    },
    constant_decl: {
      comment: "constant declaration",
      name: "meta.constant.zustral",
      match:
        "(constant)\\s+([a-zA-Z][a-zA-Z0-9_]*)\\s*(:)\\s*([a-zA-Z][a-zA-Z0-9_]*(?:\\[(.*)\\])?)",
      captures: {
        "1": {
          name: "keyword.other.zustral storage.type.constant.zustral",
        },
        "2": {
          name: "entity.name.constant.zustral",
        },
        "3": {
          name: "punctuation.colon.zustral",
        },
        "4": {
          patterns: include("#typespec"),
        },
      },
    },
    union_or_record_head: {
      comment: "top of union/record declaration",
      name: "meta.union.zustral",
      begin: "(union|record)\\s+([a-zA-Z][a-zA-Z0-9_]*)",
      end: "(?=\\bis\\b)",
      beginCaptures: {
        "1": {
          name: "keyword.declaration.$1.zustral storage.type.$1.zustral",
        },
        "2": {
          name: "entity.name.type.zustral",
        },
      },
      patterns: [
        {
          include: "#comment",
        },
        {
          comment: "type parameter list",
          begin: "\\[",
          end: "\\]",
          beginCaptures: {
            "0": {
              name: "punctuation.section.type-parameters.begin.brackets.square.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.section.type-parameters.end.brackets.square.zustral",
            },
          },
          patterns: [
            {
              include: "#context_type_parameter_list",
            },
          ],
        },
        {
          include: "#universe_assertion",
        },
      ],
    },
    record_body: {
      name: "meta.record.body.zustral",
      begin: "(?<=(record).+)(is)",
      end: "\\bend\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.record.body.begin.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.record.body.end.zustral keyword.other.end.zustral",
        },
      },
      patterns: [
        {
          include: "#context_slot_list",
        },
      ],
    },
    union_body: {
      name: "meta.record.union.zustral",
      begin: "(?<=(union).+)(is)",
      end: "\\bend\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.union.body.begin.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.union.body.end.zustral keyword.other.end.zustral",
        },
      },
      patterns: [
        {
          include: "#string_triple",
        },
        {
          include: "#comment",
        },
        {
          match: ";",
          name: "punctuation.delimiter.union.case.end.semicolon.zustral",
        },
        {
          match: "\\b(case)(?:\\s+([a-zA-Z][a-zA-Z0-9_]*))?",
          captures: {
            "1": {
              name: "keyword.declaration.case.zustral storage.type.case.zustral",
            },
            "2": {
              name: "entity.name.type.zustral",
            },
          },
        },
        {
          begin: "is",
          end: "(?=\\b(case|end)\\b)",
          beginCaptures: {
            "0": {
              name: "punctuation.definition.case.body.begin.zustral keyword.other.end.zustral",
            },
          },
          patterns: [
            {
              include: "#context_slot_list",
            },
          ],
        },
      ],
    },
    function_head: {
      name: "meta.$1-decl.zustral",
      match:
        "(function|method)\\s+([a-zA-Z][a-zA-Z0-9_]*)\\s*(?:(\\()(.*)(\\)))?\\s*(?:(:)\\s*([a-zA-Z][a-zA-Z0-9_]*(?:\\[(.*)\\])?))?",
      captures: {
        "1": {
          name: "storage.type.function.zustral",
        },
        "2": {
          name: "entity.name.function.method.zustral",
        },
        "3": {
          name: "punctuation.section.parameters.begin.bracket.round.zustral",
        },
        "4": {
          patterns: include("#context_function_paramater_def"),
        },
        "5": {
          name: "punctuation.section.parameters.end.bracket.round.zustral",
        },
        "6": {
          name: "punctuation.definition.function.return-type.zustral",
        },
        "7": {
          name: "meta.return-type.zustral",
          patterns: [
            {
              include: "#typespec",
            },
          ],
        },
      },
    },
    for_loop_head: {
      name: "meta.loop.for.zustral",
      begin: "\\bfor\\b",
      end: "(?=\\bdo\\b)",
      beginCaptures: {
        "0": {
          name: "keyword.control.for.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "keyword.control.do.zustral",
        },
      },
      patterns: [
        {
          match: "\\b(from|to)\\b",
          name: "keyword.control.$0.zustral",
        },
        {
          include: "#context_expression",
        },
      ],
    },
    while_loop_head: {
      name: "meta.loop.while.zustral",
      begin: "\\bwhile\\b",
      end: "(?=\\bdo\\b)",
      beginCaptures: {
        "0": {
          name: "keyword.control.while.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "keyword.control.do.zustral",
        },
      },
      patterns: [
        {
          include: "#context_expression",
        },
      ],
    },
    do_block: {
      name: "meta.block.do.zustral",
      begin: "\\bdo\\b",
      end: "\\bend(?:\\s+(for|while))?\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.block.begin.zustral keyword.control.$0.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.block.end.zustral keyword.control.$0.zustral",
        },
      },
      patterns: include("#context_block"),
    },
    case_head: {
      name: "meta.case.zustral",
      begin: "\\bcase\\b",
      end: "(?=\\bof\\b)",
      beginCaptures: {
        "0": {
          name: "keyword.control.$0.zustral",
        },
      },
      patterns: include("#context_expression"),
    },
    when_head: {
      name: "meta.case.when.zustral",
      begin: "\\bwhen\\b",
      end: "\\bdo\\b",
      beginCaptures: {
        "0": {
          name: "keyword.control.$0.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.block.begin.zustral keyword.control.$0.zustral",
        },
      },
      patterns: [
        {
          match: "\\b[a-zA-Z][a-zA-Z0-9_]*\\b",
          name: "entity.name.type.zustral",
        },
        {
          begin: "\\(",
          end: "\\)",
          beginCaptures: {
            "0": {
              name: "punctuation.definition.binding-pattern.begin.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.definition.binding-pattern.end.zustral",
            },
          },
          patterns: include("#context_binding_list"),
        },
      ],
    },
    borrow_head: {
      name: "meta.borrow.zustral",
      match: "(borrow\\!?)(.*)(?=do)",
      captures: {
        "1": {
          name: "keyword.other.borrow.zustral",
        },
        "2": {
          patterns: [
            {
              include: "#comment",
            },
            {
              match: "(?=in)\\s+([a-zA-Z][a-z0-9_]*)",
              name: "entity.name.type.region.zustral",
            },
            {
              match: "\\b(as|in)\\b",
              name: "keyword.other.$0.zustral",
            },
            {
              match: "[a-zA-Z][a-zA-Z0-9_]*",
              name: "variable.name.zustral",
            },
          ],
        },
      },
    },
    case_body: {
      name: "meta.body.case.zustral",
      begin: "\\bof\\b",
      end: "\\bend\\s+(case)\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.block.begin.zustral keyword.control.$0.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.block.end.zustral keyword.control.end.$1.zustral",
        },
      },
      patterns: include("#when_head", "#context_block"),
    },
    if_head: {
      comment: "head of an (else) if statement",
      name: "meta.if.zustral",
      begin: "\\b(else\\s+)?if\\b",
      end: "(?=\\bthen\\b)",
      beginCaptures: {
        "0": {
          name: "keyword.control.$0.zustral",
        },
      },
      patterns: include("#context_expression"),
    },
    if_body: {
      name: "meta.if.body.zustral",
      comment: "the body of an if/else statement",
      begin: "\\b(then|else)\\b",
      end: "\\b(?:(?=(else))|(end\\s+if))\\b",
      beginCaptures: {
        "0": {
          name: "keyword.control.$0.zustral",
        },
      },
      endCaptures: {
        "1": {
          name: "keyword.control.else.zustral",
        },
        "2": {
          name: "keyword.control.end.if.zustral",
        },
      },
      patterns: include("#context_block"),
    },
    function_body: {
      name: "meta.function.zustral",
      begin: "(?<=(function|method).+)(is)",
      end: "\\b(end)\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.block.begin.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.block.end.zustral keyword.other.end.zustral",
        },
      },
      patterns: include("#context_block"),
    },
    let_destructure: {
      name: "meta.let.destructure.zustral",
      begin: "(let)\\s+(\\{)",
      end: "\\}",
      beginCaptures: {
        "1": {
          name: "storage.type.variable.austal",
        },
        "2": {
          name: "punctuation.definition.binding-pattern.begin.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.binding-pattern.end.zustral",
        },
      },
      patterns: include("#context_binding_list"),
    },
    let_simple: {
      name: "meta.let.destructure.zustral",
      match: "(let)\\s+([a-zA-Z][a-zA-Z0-9_]*)",
      captures: {
        "1": {
          name: "storage.type.variable.austal",
        },
        "2": {
          name: "variable.other.let.zustral",
        },
      },
    },
    instance_head: {
      name: "meta.instance.zustral",
      match: "\\b(instance)\\s+([a-zA-Z][a-zA-Z0-9_]*)\\s*(\\()(.*)(\\))",
      captures: {
        "1": {
          name: "storage.type.instance.zustral",
        },
        "2": {
          name: "entity.name.type.zustral",
        },
        "3": {
          name: "punctuation.brackets.round.zustral",
        },
        "4": {
          patterns: include("#typespec"),
        },
        "5": {
          name: "punctuation.brackets.round.zustral",
        },
      },
    },
    instance_body: {
      name: "meta.instance.zustral",
      begin: "(?<=instance.+)(is)",
      end: "\\b(end)\\b",
      beginCaptures: {
        "0": {
          name: "punctuation.definition.instance.begin.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "punctuation.definition.instance.end.zustral keyword.other.end.zustral",
        },
      },
      patterns: [
        ...include("#string_triple", "#comment"),
        {
          match: "\\bfunction\\b",
          name: "invalid.illegal.function-in-instance.zustral",
        },
        ...include("#generic", "#function_head", "#function_body"),
      ],
    },
    typeclass_head: {
      comment: "top of typeclass declaration",
      name: "meta.typeclass.zustral",
      begin: "(typeclass)\\s+([a-zA-Z][a-zA-Z0-9_]*)",
      end: "(?=\\bis\\b)",
      beginCaptures: {
        "1": {
          name: "keyword.declaration.typeclass.zustral storage.type.typeclass.zustral",
        },
        "2": {
          name: "entity.name.type.zustral",
        },
      },
      patterns: [
        {
          include: "#comment",
        },
        {
          begin: "\\(",
          end: "\\)",
          beginCaptures: {
            "0": {
              name: "punctuation.section.type-parameters.begin.brackets.round.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "punctuation.section.type-parameters.end.brackets.round.zustral",
            },
          },
          patterns: include("#context_type_parameter_list"),
        },
      ],
    },
    typeclass_body: {
      comment: "typeclass declaration",
      name: "meta.typeclass.body.zustral",
      begin: "(?<=typeclass.+)(is\\b)",
      end: "\\bend\\b",
      beginCaptures: {
        "1": {
          name: "storage.type.typeclass.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "keyword.other.end.typeclass.zustral storage.type.typeclass.zustral",
        },
      },
      patterns: [
        ...include("#string_triple", "#comment"),
        {
          match: "\\bfunction\\b",
          name: "invalid.illegal.function-in-typeclass.zustral",
        },
        ...include("#generic", "#function_head"),
      ],
    },
    module: {
      patterns: [
        {
          comment: "module interface",
          name: "meta.module.interface.zustral",
          begin:
            "(module)\\s+([a-zA-Z][a-zA-Z0-9_]*(?:\\.[a-zA-Z][a-zA-Z0-9_]*)*)\\s+(is)",
          end: "\\bend\\s+module\\b",
          beginCaptures: {
            "1": {
              name: "storage.type.module.zustral",
            },
            "2": {
              name: "entity.name.module.zustral",
            },
            "3": {
              name: "storage.type.module.zustral keyword.other.is.zustral",
            },
          },
          endCaptures: {
            "0": {
              name: "keyword.other.end.module.interface.zustral storage.type.module.interface.zustral",
            },
          },
          patterns: include("#context_module_interface"),
        },
      ],
    },
    module_body: {
      comment: "module body",
      name: "meta.module.body.zustral",
      begin:
        "(module\\s+body)\\s+([a-zA-Z][a-zA-Z0-9_]*(?:\\.[a-zA-Z][a-zA-Z0-9_]*)*)\\s+(is)",
      end: "\\bend\\s+module\\s+body\\b",
      beginCaptures: {
        "1": {
          name: "storage.type.module.zustral",
        },
        "2": {
          name: "entity.name.module.zustral",
        },
        "3": {
          name: "storage.type.module.zustral keyword.other.is.zustral",
        },
      },
      endCaptures: {
        "0": {
          name: "keyword.other.end.module.body.zustral storage.type.module.body.zustral",
        },
      },
      patterns: include("#context_module_body"),
    },
    escapes: {
      comment: "string escape characters",
      name: "meta.string.escape.zustral",
      patterns: [
        {
          name: "constant.character.escape.zustral",
          match: "\\\\(n|r|t|\\\\| |')",
        },
        {
          comment: "mark unmatched escapes as illegal",
          name: "invalid.illegal.escape-sequence.zustral",
          match: "\\\\.",
        },
      ],
    },
    string: {
      name: "string.quoted.double.zustral",
      begin: '"',
      end: '"',
      patterns: [
        {
          name: "constant.character.escape.zustral",
          match: '\\\\"',
        },
        {
          include: "#escapes",
        },
      ],
    },
    string_triple: {
      name: "string.quoted.triple.zustral",
      begin: '"""',
      end: '"""',
      patterns: [
        {
          name: "constant.character.escape.zustral",
          match: '\\\\"""',
        },
        {
          include: "#escapes",
        },
      ],
    },
  },
  scopeName: "source.zustral",
};

export default language;
