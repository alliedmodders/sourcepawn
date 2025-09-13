# vim: set sts=2 ts=8 sw=2 tw=99 et:
import os
import sys
import yaml
import locale
import pymysql
import argparse
import subprocess
import json as JSON

try:
  dorange = xrange
except:
  dorange = range

def DecodeConsoleText(origin, text):
  try:
    if origin.encoding:
      return text.decode(origin.encoding, 'replace')
  except:
    pass
  try:
    return text.decode(locale.getpreferredencoding(), 'replace')
  except:
    pass
  return text.decode('utf8', 'replace')

class Comment(object):
  def __init__(self):
    super(Comment, self).__init__()
    self.main = ''
    self.tags = []

class CommentParser(object):
  def __init__(self, text):
    self.text = text
    self.pos = 0
    self.doc = Comment()

  def peekChar(self):
    if self.pos >= len(self.text):
      return '\0'
    return self.text[self.pos]
  def getChar(self):
    if self.pos >= len(self.text):
      return '\0'
    self.pos += 1
    return self.text[self.pos - 1]
  def matchChar(self, c):
    if self.peekChar() != c:
      return False
    return self.getChar()

  def parse(self):
    while self.pos < len(self.text):
      if self.getChar() == '/':
        if self.matchChar('*'):
          self.parse_multiline()
        elif self.matchChar('/'):
          self.parse_singleline()
    return self.doc

  def parse_multiline(self):
    # Find the body of the comment.
    body_start = self.pos
    body_end = None
    while self.pos < len(self.text):
      if self.getChar() == '*':
        if self.getChar() == '/':
          body_end = self.pos - 2
          break
    if body_end is None:
      body_end = self.pos

    # Break the comment into lines.
    lines = self.text[body_start:body_end].splitlines()

    # Strip leading **<
    lines = [line.lstrip() for line in lines]
    lines = [line.lstrip('*') for line in lines]
    lines = [line.lstrip('<') for line in lines]

    # Strip whitespace and *.
    lines = [line.strip(' \v\t') for line in lines]
    lines = [line.replace('\t', ' ') for line in lines]
    self.parse_lines(lines)

  def parse_singleline(self):
    # Find the end of the single-line block.
    body_start = self.pos
    body_end = None
    first_char = True
    while self.pos < len(self.text):
      c = self.getChar()
      if c == '\r' or c == '\n':
        first_char = True
        continue
      if c.isspace() or not first_char:
        continue

      first_char = False
      if c == '/':
        if not self.matchChar('/'):
          body_end = self.pos - 1
          break
    if body_end is None:
      body_end = self.pos

    # Break the comment into lines.
    rawlines = self.text[body_start:body_end].splitlines()
    lines = []
    for line in rawlines:
      # Strip comment parts.
      line = line.lstrip()
      if line.startswith('//'):
        line = line.lstrip('/')

      # Strip whitespace.
      line = line.strip()
      line = line.replace('\t', ' ')
      lines.append(line)

    self.parse_lines(lines)

  def parse_lines(self, lines):
    block_tag = None
    block_lines = []
    for index, line in enumerate(lines):
      if line.startswith('@'):
        tag_end = line.find(' ', 1)
        if tag_end == -1 or tag_end == 1:
          continue

        if index != 0:
          self.push_block(block_tag, block_lines)

        block_tag = line[1:tag_end]
        block_lines = []
        line = line[tag_end+1:].strip()

        if block_tag == 'param':
          param_end = line.find(' ')
          if param_end != -1:
            block_tag += ':' + line[:param_end]
            line = line[param_end+1:].strip()
          else:
            block_tag += ':unknown'
      block_lines.append(line)
    self.push_block(block_tag, block_lines)

  def push_block(self, tag, lines):
    # Trim front and back empty lines.
    while len(lines) and not len(lines[len(lines) - 1]):
      lines.pop()
    while len(lines) and not len(lines[0]):
      lines = lines[1:]
    if not len(lines):
      return

    for index, line in enumerate(lines):
      if not len(line):
        lines[index] = '\n'

    text = ' '.join(lines)
    if tag is None or tag == 'brief':
      if self.doc.main:
        self.doc.main += '\n'
      self.doc.main += text
    else:
      self.doc.tags.append((tag, text))

class DocGen(object):
  def __init__(self, config):
    super(DocGen, self).__init__()
    self.config = config
    self.db = pymysql.connect(
      host = self.config['database']['host'],
      user = self.config['database']['user'],
      passwd = self.config['database']['pass'],
      db = self.config['database']['name'])
    self.current_class = None

  def generate(self):
    for include in os.listdir(self.config['includes']):
      if not include.endswith('.inc'):
        continue
      include = include[:len(include) - 4]

      self.parse_include(include)

    self.db.commit()

  def parse_include(self, include):
    argv = [
      self.config['parser'],
      os.path.join(self.config['includes'], include + '.inc'),
    ]
    p = subprocess.Popen(
      args = argv,
      stdout = subprocess.PIPE,
      stderr = subprocess.PIPE)
    stdoutData, stderrData = p.communicate()
    stdout = DecodeConsoleText(sys.stdout, stdoutData).strip()
    stderr = DecodeConsoleText(sys.stderr, stderrData).strip()
    if p.returncode != 0:
      print('Failed to process {0}:'.format(argv[1]))
      print(stderr)
      raise Exception('failed to parse file')

    if len(stderr) > 0:
      print('Notes for {0}:'.format(argv[1]))
      print(stderr)

    with open(argv[1], 'rb') as fp:
      self.current_file = fp.read()

    json = JSON.loads(stdout)

    query = """
      insert into spdoc_include
        (name, doc, content)
      values
        (%s,   %s,  %s)
      on duplicate key update
        id = last_insert_id(id),
        doc = %s,
        content = %s
    """
    cn = self.db.cursor()
    cn.execute(query, (
      include,
      '', self.current_file,
      '', self.current_file))
    self.current_include = cn.lastrowid

    self.parse_classes(json['methodmaps'])
    self.parse_constants(json['constants'])
    self.parse_functions(json['functions'])
    self.parse_enums(json['enums'])
    self.parse_typesets(json['typesets'])
    self.parse_typedefs(json['typedefs'])
    self.current_include = None
    self.current_file = None

  def parse_doc(self, obj):
    if 'docStart' not in obj:
      return Comment()

    docstart = obj['docStart']
    docend = obj['docEnd']
    text = self.current_file[docstart:docend]

    parser = CommentParser(text)
    return parser.parse()

  def parse_classes(self, methodmaps):
    for methodmap in methodmaps:
      self.parse_class(methodmap)
  def parse_constants(self, constants):
    for constant in constants:
      self.parse_constant(constant, None, 0)
  def parse_functions(self, functions):
    for function in functions:
      self.parse_function(function)
  def parse_enums(self, enums):
    for enum in enums:
      self.parse_enum(enum)
  def parse_typesets(self, typesets):
    for typeset in typesets:
      self.parse_typeset(typeset)
  def parse_typedefs(self, types):
    for typeinfo in types:
      self.parse_typedef(typeinfo)

  def parse_class(self, layout):
    doc = self.parse_doc(layout)

    data = JSON.dumps({ 'tags': doc.tags })

    query = """
      insert into spdoc_class
        (include_id, name, brief, data)
      values
        (%s, %s, %s, %s)
      on duplicate key update
        id = last_insert_id(id),
        brief = %s,
        data = %s
    """
    cn = self.db.cursor()
    cn.execute(query, (
      self.current_include,
      layout['name'],
      doc.main, data,
      doc.main, data))

    self.current_class = cn.lastrowid
    for method in layout['methods']:
      self.parse_function(method)
    for property in layout['properties']:
      self.parse_property(property)
    self.current_class = None

  def parse_property(self, property):
    doc = self.parse_doc(property)
    data = { 'tags': doc.tags }

    update = (
      property['type'],
      property['getter'],
      property['setter'],
      doc.main,
      JSON.dumps(data),
    )
    query_args = (
      self.current_include,
      self.current_class,
      property['name']) + update + update

    query = """
      insert into spdoc_property
        (include_id, class_id, name, type, getter, setter, brief, data)
      values
        (%s,         %s,       %s,   %s,   %s,     %s,     %s,    %s)
      on duplicate key update
        id = last_insert_id(id),
        type = %s,
        getter = %s,
        setter = %s,
        brief = %s,
        data = %s
    """
    cn = self.db.cursor()
    cn.execute(query, query_args)

  def parse_constant(self, constant, parent_type, parent_id):
    doc = self.parse_doc(constant)
    data = JSON.dumps({ 'tags': doc.tags })

    query = """
      insert into spdoc_constant
        (include_id, parent_type, parent_id, name, brief, data)
      values
        (%s,         %s,          %s,        %s,   %s,    %s)
      on duplicate key update
        id = last_insert_id(id),
        brief = %s,
        data = %s
    """
    cn = self.db.cursor()
    cn.execute(query, (
      self.current_include,
      parent_type,
      parent_id,
      constant['name'],
      doc.main, data,
      doc.main, data))

  def parse_enum(self, enum):
    doc = self.parse_doc(enum)
    data = JSON.dumps({ 'tags': doc.tags })

    query = """
      insert into spdoc_enum
        (include_id, name, brief, data)
      values
        (%s,         %s,   %s,    %s)
      on duplicate key update
        id = last_insert_id(id),
        brief = %s,
        data = %s
    """
    cn = self.db.cursor()
    cn.execute(query, (
      self.current_include,
      enum['name'],
      doc.main, data,
      doc.main, data))

    enum_id = cn.lastrowid
    for entry in enum['entries']:
      self.parse_constant(entry, 'enum', enum_id)

  def parse_typedef(self, typeinfo):
    doc = self.parse_doc(typeinfo)
    data = JSON.dumps({
      'brief': doc.main,
      'tags': doc.tags,
      'type': typeinfo['type']
    })

    old_id = self.find_old('spdoc_type', {
      'include_id': self.current_include,
      'parent_type': None,
      'parent_id': None,
      'name': typeinfo['name']
    })

    cn = self.db.cursor()
    if old_id is None:
      query = """
        insert into spdoc_type
          (include_id, parent_type, parent_id, kind, name, brief, data)
        values
          (%s,         %s,          %s,        %s,   %s,   %s,    %s)
      """
      cn.execute(query, (
        self.current_include,
        None, None,
        'typedef',
        typeinfo['name'],
        doc.main, data))
    else:
      query = """
        update spdoc_type
        set
          parent_type = %s,
          parent_id = %s,
          kind = 'typedef',
          brief = %s,
          data = %s
        where
          id = %s
      """
      cn.execute(query, (None, None, doc.main, data, old_id))

  def parse_typeset(self, typeset):
    doc = self.parse_doc(typeset)

    types = []
    for typeinfo in typeset['types']:
      comment = self.parse_doc(typeinfo)
      types.append({
        'brief': comment.main,
        'tags': comment.tags,
        'type': typeinfo['type']
      })

    data = JSON.dumps({
      'tags': doc.tags,
      'types': types,
    })

    old_id = self.find_old('spdoc_type', {
      'include_id': self.current_include,
      'parent_type': None,
      'parent_id': None,
      'name': typeset['name']
    })

    cn = self.db.cursor()
    if old_id is None:
      query = """
        insert into spdoc_type
          (include_id, parent_type, parent_id, kind, name, brief, data)
        values
          (%s,         %s,          %s,        %s,   %s,   %s,    %s)
      """
      cn.execute(query, (
        self.current_include,
        None, None,
        'typeset',
        typeset['name'],
        doc.main, data))
    else:
      query = """
        update spdoc_type
        set
          parent_type = %s,
          parent_id = %s,
          kind = 'typeset',
          brief = %s,
          data = %s
        where
          id = %s
      """
      cn.execute(query, (None, None, doc.main, data, old_id))

  def parse_function(self, function):
    doc = self.parse_doc(function)
    data = {
      'tags': [],
      'return': {
        'type': function['returnType'],
        'doc': '',
      },
      'params': [],
    }

    # Parse documentation tags.
    params = {}
    for tag, text in doc.tags:
      if tag == 'noreturn':
        del data['return']
        continue
      if tag == 'return':
        data['return']['doc'] = text
        continue
      if tag == 'error':
        data['error'] = text
        continue
      if tag.startswith('param:'):
        params[tag] = text
        continue

      # Anything unrecognized we add to the top.
      data['tags'].append({'tag': tag, 'text': text})

    for argument in function['arguments']:
      param = {
        'name': argument['name'],
        'type': argument['type'],
      }
      param['doc'] = params.get('param:' + argument['name'], '')
      data['params'].append(param)

    signature = '{0} {1}({2})'.format(
      function['returnType'],
      function['name'],
      ', '.join([arg['decl'] for arg in function['arguments']]))

    if function['returnType'] == 'void':
      del data['return']

    json = JSON.dumps(data)

    if self.current_class is not None:
      parent_type = 'class'
    else:
      parent_type = None

    old_id = self.find_old('spdoc_function', {
      'include_id': self.current_include,
      'parent_type': parent_type,
      'parent_id': self.current_class,
      'name': function['name']
    })

    cn = self.db.cursor()
    if old_id is None:
      query = """
        insert into spdoc_function
          (include_id, parent_type, parent_id, kind, name, signature, brief, data)
        values
          (%s, %s, %s, %s, %s, %s, %s, %s)
      """
      cn.execute(query, (
        self.current_include,
        parent_type,
        self.current_class,
        function.get('kind'),
        function['name'],
        signature, doc.main, json))
    else:
      query = """
        update spdoc_function
        set
          signature = %s,
          brief = %s,
          data = %s
        where
          id = %s
      """
      cn.execute(query, (signature, doc.main, json, old_id))

  def find_old(self, table, args):
    columns = []
    values = []
    for key in args:
      value = args[key]
      if value is None:
        columns.append('{0} IS NULL'.format(key))
      else:
        columns.append('{0} = %s'.format(key))
        values.append(value)

    query = 'SELECT id FROM {0} WHERE {1}'.format(
      table,
      ' AND '.join(columns))
    cn = self.db.cursor()
    cn.execute(query, values)
    row = cn.fetchone()
    if row is None:
      return None
    return row[0]

def main():
  ap = argparse.ArgumentParser()
  ap.add_argument('-c', '--config', type=str, default='config.yml',
                  help='Config file')
  args = ap.parse_args()

  with open(args.config) as fp:
    config = yaml.load(fp)

  gen = DocGen(config)
  gen.generate()

def test():
  x = """
  /**
   * @brief Hello
   *
   * @param Egg This is an egg.
   *            alos, an egg.
   */
  """
  x = """
   // @brief Hello
   // 
   // @param Egg This is an egg.
   //            alos, an egg.
   // 
  """
  parser = CommentParser(x)
  doc = parser.parse()
  print(doc.main, doc.tags)

if __name__ == '__main__':
  main()
