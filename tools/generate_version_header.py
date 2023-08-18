# vim: set ts=4 sw=4 tw=99 et:
import argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('version_file', type = str)
    parser.add_argument('output', type = str)
    args = parser.parse_args()

    with open(args.version_file, 'rt') as fp:
        version = fp.read().strip()

    parts = version.split('.')
    while len(parts) < 4:
        parts.append('0')
    file_version = ','.join(parts)

    with open(args.output, 'wt') as fp:
        fp.write("#define SM_VERSION_FILE {}\n".format(file_version))
        fp.write("#define SM_VERSION_STRING \"{}\"\n".format(version))

if __name__ == '__main__':
    main()