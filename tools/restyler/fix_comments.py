# vim: set sts=2 ts=2 sw=2 tw=99 et:
import argparse
from collections import deque
from collections import namedtuple
from pygments.token import Token as TokenKind
from pygments.lexers.c_cpp import CppLexer

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('-i', '--in-place', action = 'store_true', default = False,
            help = 'Modify in-place instead of write to stdout')
  parser.add_argument('file', type = str, nargs = '+',
            help = 'Files to fix')
  args = parser.parse_args()

  for file in args.file:
    process_file(file, args)

def process_file(path, args):
  with open(path, 'rb') as fp:
    text = fp.read().decode('utf-8')

  lexer = CppLexer()
  tokens = lexer.get_tokens_unprocessed(text)
  fixup_tool = FixupTool(tokens)
  changes = fixup_tool.parse()
  new_text = change_text(text, changes)

  if args.in_place:
    with open(path, 'wt') as fp:
      fp.write(new_text)
  else:
    print(new_text)

Token = namedtuple('Token', ['pos', 'kind', 'text', 'line', 'col'])

def change_text(text, changes):
  characters = list(text)
  additions = []
  for cmd, tok, c in changes:
    if cmd == 'replace':
      characters[tok.pos] = c
    elif cmd == 'insert':
      additions.append((tok.pos, c))
    elif cmd == 'insert-before':
      additions.append((tok.pos - 1, c))
    elif cmd == 'delete':
      for i in range(len(tok.text)):
        characters[tok.pos + i] = ''
    elif cmd == 'truncate-line-at':
      # Remove the given character and all preceding whitespace.
      characters[tok.pos] = ''
      pos = tok.pos - 1
      while pos >= 0 and characters[pos] == ' ':
        characters[pos] = ''
        pos -= 1
    else:
      raise Exception('not yet implemented')

  # Process additions in reverse to make this algorithm easier.
  additions = sorted(additions, key=lambda entry: entry[0])
  while additions:
    pos, c = additions.pop()
    characters.insert(pos + 1, c)
  return ''.join(characters)

class FixupTool(object):
  def __init__(self, tokens):
    self.tokens_ = tokens
    self.raw_readahead_ = deque()
    self.changes_ = []
    self.line_ = 1
    self.line_indent_ = ''
    self.line_pos_ = 0
    self.determined_indent_ = False
    self.prev_comment_ = None

  def parse(self):
    while True:
      tok = self.get_next()
      if not tok:
        break
      if tok.kind != TokenKind.Comment.Multiline:
        continue
      self.analyze_comment(tok)

    return self.changes_

  kPointlessComments = set([
    '/* while */',
    '/* if */',
    '/* switch */',
    '/* for */',
    '/* do */',
  ])
  def analyze_comment(self, tok):
    if tok.text in FixupTool.kPointlessComments:
      assert self.prev_comment_ is None
      self.delete(tok)
      return

    if not tok.text.endswith('*/'):
      self.prev_comment_ = tok
      return
    if not self.prev_comment_:
      return

    prev_comment = self.prev_comment_
    self.prev_comment_ = None

    #if tok.text.startswith(' '):
    #  return

  def delete(self, tok):
    self.changes_.append(('delete', tok, None))

  # Read the next raw token from the lexer, potentially splitting it into
  # multiple tokens if needed.
  def read_next_raw_token(self):
    if self.raw_readahead_:
      return self.raw_readahead_.popleft()

    pos, kind, text = next(self.tokens_)

    # Split newlines into separate tokens to make things more accurate.
    if '\n' in text:
      assert (kind in TokenKind.Comment) or (kind == TokenKind.Text)
      for piece in split_text_by_newlines(text):
        if piece == '\n':
          self.raw_readahead_.append((pos, TokenKind.Text, piece))
        else:
          self.raw_readahead_.append((pos, kind, piece))
        pos += len(piece)
      return self.raw_readahead_.popleft()

    # Otherwise, return the token unadultered.
    return pos, kind, text

  # Read the next raw token, but don't consume it.
  def peek_next_raw_token(self):
    try:
      pos, kind, text = self.read_next_raw_token()
      self.raw_readahead_.appendleft((pos, kind, text))
      return pos, kind, text
    except StopIteration:
      return None, None, None

  # Convert the next raw token into a full-fledged Token object, which has line
  # information. By snooping at lower-level tokens we can determine the
  # indentation level.
  def get_next(self):
    try:
      pos, kind, text = self.read_next_raw_token()
      tok = Token(pos, kind, text, self.line_, self.line_pos_)
      self.analyze_lines(tok)
      return tok
    except StopIteration:
      return None

  # Determine the line state from the new token.
  def analyze_lines(self, tok):
    if tok.kind == TokenKind.Text:
      assert (tok.text.endswith('\n') and tok.text.count('\n') <= 1) or \
             '\n' not in tok.text
    if tok.text == '\n':
      self.line_ += 1
      self.line_indent_ = ''
      self.line_pos_ = 0
      self.determined_indent_ = False
      self.first_token_on_line_ = None
      return

    if tok.kind == TokenKind.Text and tok.text.isspace() and not self.determined_indent_:
      self.line_indent_ += tok.text
    elif not self.determined_indent_:
      self.determined_indent_ = True
      self.first_token_on_line_ = tok
    self.line_pos_ += len(tok.text)

def split_text_by_newlines(text):
  pieces = text.split('\n')
  output = []
  for index, piece in enumerate(pieces):
    if len(piece) > 0:
      output.append(piece)
    if index != len(pieces) - 1:
      output.append('\n')
  return output

if __name__ == '__main__':
  main()
