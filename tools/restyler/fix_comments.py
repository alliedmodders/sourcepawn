# vim: set sts=2 ts=2 sw=2 tw=99 et:
import argparse
import subprocess
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

  nl_style = determine_nl_style(path)

  lexer = CppLexer()
  tokens = lexer.get_tokens_unprocessed(text)
  fixup_tool = FixupTool(path, tokens, nl_style)
  changes = fixup_tool.parse()
  new_text = change_text(text, changes)

  if args.in_place:
    with open(path, 'wt') as fp:
      fp.write(new_text)
  else:
    print(new_text)

def determine_nl_style(path):
  output = subprocess.check_output(['file', path])
  output = output.decode('utf-8')
  if 'CRLF' in output:
    return '\r\n'
  return '\n'

Token = namedtuple('Token', ['pos', 'kind', 'text', 'line', 'col'])

def change_text(text, changes):
  characters = list(text)
  additions = []
  for cmd, start_pos, count, text in changes:
    if cmd == 'delete':
      for i in range(count):
        print(cmd, start_pos + i, count, characters[start_pos + i])
        characters[start_pos + i] = ''
    elif cmd == 'insert-after':
      additions.append((start_pos, text))
    elif cmd == 'delete-ws':
      for i in reversed(range(start_pos, start_pos + count)):
        if not characters[i].isspace():
          break
        characters[i] = ''
    else:
      raise Exception('unknown command: {0}'.format(cmd))

  # Process additions in reverse to make this algorithm easier.
  additions = sorted(additions, key=lambda entry: entry[0])
  while additions:
    pos, c = additions.pop()
    characters.insert(pos + 1, c)
  return ''.join(characters)

class FixupTool(object):
  def __init__(self, file, tokens, nl_style):
    self.file_ = file
    self.tokens_ = tokens
    self.raw_readahead_ = deque()
    self.changes_ = []
    self.line_ = 1
    self.line_indent_ = ''
    self.line_pos_ = 0
    self.determined_indent_ = False
    self.root_comment_ = None
    self.nl_style_ = nl_style

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
      assert self.root_comment_ is None
      self.delete(tok)
      return

    if tok.text.startswith('/*'):
      self.root_comment_ = tok
      return

    # For some unknown reason, clang-format is unable to properly format
    # multi-line comments, and leaves them a misaligned mess. Fix that up here.
    maybe_asterisk = self.find_first_non_whitespace(tok.text)
    if maybe_asterisk is None:
      return
    delta = self.root_comment_.col + 1 - maybe_asterisk
    if tok.text[maybe_asterisk] != '*':
      delta += 2
    if delta == 0:
      return
    if delta > 0:
      extra_spaces = delta * ' '
      self.insert_after(tok.pos + maybe_asterisk - 1, extra_spaces)
    else:
      self.delete_ws(maybe_asterisk - delta - 1, 0-delta)

  def find_first_non_whitespace(self, text):
    index = 0
    while index < len(text):
      if text[index].isspace():
        index += 1
        continue
      return index

  def delete(self, tok):
    self.changes_.append(('delete', tok.pos, len(tok.text), None))

  def delete_ws(self, pos, len):
    self.changes_.append(('delete-ws', pos, len, None))

  def insert_after(self, pos, text):
    assert pos >= 0
    self.changes_.append(('insert-after', pos, None, text))

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
