# vim: set ts=8 sts=4 sw=4 tw=99 et:
import argparse
import os
import subprocess
import urllib.parse
import yaml

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('manifest', type = str, help = 'Manifest file')
    parser.add_argument('output', type = str, help = 'Output folder')
    args = parser.parse_args()

    with open(args.manifest, 'rb') as fp:
        manifest = yaml.load(fp, Loader = yaml.Loader)

    work = {}
    file_lists = {}
    for entry in manifest['git']:
        repo_url, source_file = entry.split(' ')
        r = urllib.parse.urlparse(repo_url)

        assert r.path.startswith('/')
        repo_path = r.path[1:]

        if r.netloc not in file_lists:
            file_lists[r.netloc] = []
        file_lists[r.netloc].append(os.path.join(repo_path, source_file))

        if repo_url not in work:
            work[repo_url] = {
                'forge': r.netloc,
                'path': repo_path,
            }

    for repo in work:
        DownloadOrUpdate(args, repo, work[repo])

    for forge in file_lists:
        out_manifest = os.path.join(args.output, forge, 'corpus.list')
        with open(out_manifest, 'wt') as fp:
            for entry in file_lists[forge]:
                fp.write('{}\n'.format(entry))
        print('Wrote test list to {}'.format(out_manifest))

def DownloadOrUpdate(args, repo_url, info):
    output_path = os.path.join(args.output, info['forge'], info['path'])
    if os.path.exists(output_path):
        try:
            subprocess.check_call(['git', 'pull'], cwd = output_path)
        except:
            print('Could not update {}'.format(repo_url))
        return

    output_dir = os.path.dirname(output_path)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    try:
        subprocess.check_call(['git', 'clone', '--recursive', repo_url], cwd = output_dir)
    except:
        print('Could not download {}'.format(repo_url))

if __name__ == '__main__':
    main()
