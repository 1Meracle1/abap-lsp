#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdlib.h>

enum TokenType {
  STAR_COMMENT,
  TEMPLATE_START,
  TEMPLATE_TEXT,
  TEMPLATE_INTERPOLATION_START,
  TEMPLATE_INTERPOLATION_END,
  TEMPLATE_END,
};

typedef struct {
  unsigned template_depth;
  unsigned interpolation_depth;
} Scanner;

void *tree_sitter_abap_external_scanner_create(void) {
  return calloc(1, sizeof(Scanner));
}

void tree_sitter_abap_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_abap_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  buffer[0] = (char)scanner->template_depth;
  buffer[1] = (char)scanner->interpolation_depth;
  return 2;
}

void tree_sitter_abap_external_scanner_deserialize(
  void *payload,
  const char *buffer,
  unsigned length
) {
  Scanner *scanner = (Scanner *)payload;
  scanner->template_depth = 0;
  scanner->interpolation_depth = 0;

  if (length >= 2) {
    scanner->template_depth = (unsigned char)buffer[0];
    scanner->interpolation_depth = (unsigned char)buffer[1];
  }
}

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

static bool scan_star_comment(TSLexer *lexer) {
  if (lexer->lookahead != '*' || lexer->get_column(lexer) != 0) {
    return false;
  }

  while (lexer->lookahead != 0 && lexer->lookahead != '\n' && lexer->lookahead != '\r') {
    advance(lexer);
  }

  lexer->mark_end(lexer);
  lexer->result_symbol = STAR_COMMENT;
  return true;
}

static bool scan_template_start(Scanner *scanner, TSLexer *lexer) {
  if (lexer->lookahead != '|') {
    return false;
  }

  advance(lexer);
  lexer->mark_end(lexer);
  scanner->template_depth++;
  lexer->result_symbol = TEMPLATE_START;
  return true;
}

static bool scan_template_end(Scanner *scanner, TSLexer *lexer) {
  if (scanner->template_depth == 0 || lexer->lookahead != '|') {
    return false;
  }

  advance(lexer);
  lexer->mark_end(lexer);
  scanner->template_depth--;

  if (scanner->interpolation_depth > scanner->template_depth) {
    scanner->interpolation_depth = scanner->template_depth;
  }

  lexer->result_symbol = TEMPLATE_END;
  return true;
}

static bool scan_template_delimiter(
  Scanner *scanner,
  TSLexer *lexer,
  int delimiter,
  enum TokenType token_type
) {
  if (lexer->lookahead != delimiter) {
    return false;
  }

  if (token_type == TEMPLATE_INTERPOLATION_START) {
    if (scanner->template_depth <= scanner->interpolation_depth) {
      return false;
    }
  } else if (token_type == TEMPLATE_INTERPOLATION_END) {
    if (
      scanner->interpolation_depth == 0 ||
      scanner->template_depth != scanner->interpolation_depth
    ) {
      return false;
    }
  }

  advance(lexer);
  lexer->mark_end(lexer);

  if (token_type == TEMPLATE_INTERPOLATION_START) {
    scanner->interpolation_depth++;
  } else if (token_type == TEMPLATE_INTERPOLATION_END) {
    scanner->interpolation_depth--;
  }

  lexer->result_symbol = token_type;
  return true;
}

static bool scan_template_text(Scanner *scanner, TSLexer *lexer) {
  if (scanner->template_depth <= scanner->interpolation_depth) {
    return false;
  }

  bool consumed = false;

  while (lexer->lookahead != 0 &&
         lexer->lookahead != '\n' &&
         lexer->lookahead != '\r' &&
         lexer->lookahead != '|' &&
         lexer->lookahead != '{' &&
         lexer->lookahead != '}') {
    if (lexer->lookahead == '\\') {
      advance(lexer);
      consumed = true;

      if (lexer->lookahead == 0 || lexer->lookahead == '\n' || lexer->lookahead == '\r') {
        lexer->mark_end(lexer);
        lexer->result_symbol = TEMPLATE_TEXT;
        return consumed;
      }
    }

    advance(lexer);
    consumed = true;
    lexer->mark_end(lexer);
  }

  if (!consumed) {
    return false;
  }

  lexer->result_symbol = TEMPLATE_TEXT;
  return true;
}

bool tree_sitter_abap_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  Scanner *scanner = (Scanner *)payload;

  if (!valid_symbols[TEMPLATE_TEXT]) {
    while (
      lexer->lookahead == ' ' ||
      lexer->lookahead == '\t' ||
      lexer->lookahead == '\n' ||
      lexer->lookahead == '\r'
    ) {
      skip(lexer);
    }
  }

  if (valid_symbols[STAR_COMMENT] && scan_star_comment(lexer)) {
    return true;
  }

  if (valid_symbols[TEMPLATE_END] && scan_template_end(scanner, lexer)) {
    return true;
  }

  if (valid_symbols[TEMPLATE_START] && scan_template_start(scanner, lexer)) {
    return true;
  }

  if (
    valid_symbols[TEMPLATE_INTERPOLATION_END] &&
    scan_template_delimiter(scanner, lexer, '}', TEMPLATE_INTERPOLATION_END)
  ) {
    return true;
  }

  if (
    valid_symbols[TEMPLATE_INTERPOLATION_START] &&
    scan_template_delimiter(scanner, lexer, '{', TEMPLATE_INTERPOLATION_START)
  ) {
    return true;
  }

  if (valid_symbols[TEMPLATE_TEXT] && scan_template_text(scanner, lexer)) {
    return true;
  }

  return false;
}
