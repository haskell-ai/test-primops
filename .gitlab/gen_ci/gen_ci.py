#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p curl  "python3.withPackages (ps:[ps.pyyaml ps.python-gitlab ])"

"""
"""

from subprocess import run, check_call
from getpass import getpass
import shutil
from pathlib import Path
from typing import NamedTuple, Callable, List, Dict, Optional
import tempfile
import re
import pickle
import os
import yaml
import gitlab
from urllib.request import urlopen
import hashlib
import sys
import json
import urllib.parse

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


gl = gitlab.Gitlab('https://gitlab.haskell.org', per_page=100)



# Artifact precisely specifies a job what the bindist to download is called.
class Artifact(NamedTuple):
    job_name: str
    download_name: str
    output_name: str
    subdir: str

# Platform spec provides a specification which is agnostic to Job
# PlatformSpecs are converted into Artifacts by looking in the jobs-metadata.json file.
class PlatformSpec(NamedTuple):
    name: str
    subdir: str

source_artifact = Artifact('source-tarball'
                          , 'ghc-{version}-src.tar.xz'
                          , 'ghc-{version}-src.tar.xz'
                          , 'ghc-{version}' )
test_artifact = Artifact('source-tarball'
                        , 'ghc-{version}-testsuite.tar.xz'
                        , 'ghc-{version}-testsuite.tar.xz'
                        , 'ghc-{version}' )

def debian(arch, n):
    return linux_platform(arch, "{arch}-linux-deb{n}".format(arch=arch, n=n))

def darwin(arch):
    return PlatformSpec ( '{arch}-darwin'.format(arch=arch)
                        , 'ghc-{version}-{arch}-apple-darwin'.format(arch=arch, version="{version}") )

windowsArtifact = PlatformSpec ( 'x86_64-windows'
                               , 'ghc-{version}-x86_64-unknown-mingw32' )

def centos(n):
    return linux_platform("x86_64", "x86_64-linux-centos{n}".format(n=n))

def fedora(n):
    return linux_platform("x86_64", "x86_64-linux-fedora{n}".format(n=n))

def alpine(n):
    return linux_platform("x86_64", "x86_64-linux-alpine{n}".format(n=n))

def rocky(n):
    return linux_platform("x86_64", "x86_64-linux-rocky{n}".format(n=n))

def ubuntu(n):
    return linux_platform("x86_64", "x86_64-linux-ubuntu{n}".format(n=n))

def linux_platform(arch, opsys):
    return PlatformSpec( opsys, 'ghc-{version}-{arch}-unknown-linux'.format(version="{version}", arch=arch) )


base_url = 'https://gitlab.haskell.org/api/v4/projects/1/jobs/{job_id}/artifacts/{artifact_name}'


# Turns a platform into an Artifact respecting pipeline_type
# Looks up the right job to use from the .gitlab/jobs-metadata.json file
def mk_from_platform(job_mapping, pipeline_type, platform):
    info = job_mapping[platform.name][pipeline_type]
    eprint(f"From {platform.name} / {pipeline_type} selecting {info['name']}")
    return Artifact(info['name']
                   , f"{info['jobInfo']['bindistName']}.tar.xz"
                   , "ghc-{version}-{pn}.tar.xz".format(version="{version}", pn=platform.name)
                   , platform.subdir)


# Generate the new metadata for a specific GHC mode etc
def mk_new_yaml(job_map, job_mapping, pipeline_type, version):
    def mk(platform):
        eprint("\n=== " + platform.name + " " + ('=' * (75 - len(platform.name))))
        artifact = mk_from_platform(job_mapping, pipeline_type, platform)

        job_id = job_map[artifact.job_name].id
        url = base_url.format(job_id=job_id, artifact_name=urllib.parse.quote_plus(artifact.download_name.format(version=version)))

        eprint(artifact)
        eprint(url)
        job ={ platform.name + "-upstream" :
                { "extends" : ".build-" + platform.name
                 , "variables" : { "BINDIST_URL" : url } }
             }
        return job


    platforms = [debian("x86_64", 12), debian("aarch64", 12), darwin("aarch64"), darwin("x86_64")]
    result = {}
    for platform in platforms:
        result.update(mk(platform))

    return result




def artifact_url (job_map, job_name, artifact_name):
    job_id = job_map[job_name].id
    url = base_url.format(job_id=job_id, artifact_name=urllib.parse.quote_plus(artifact_name))
    return url



def main() -> None:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--pipeline-id', required=True, type=int, help='Which pipeline to generate jobs for')
    args = parser.parse_args()


    project = gl.projects.get(1, lazy=True)
    pipeline = project.pipelines.get(args.pipeline_id)
    jobs = pipeline.jobs.list()
    job_map = { job.name: job for job in jobs }
    # Bit of a hacky way to determine what pipeline we are dealing with but
    # the aarch64-darwin job should stay stable for a long time.
    if 'nightly-aarch64-darwin-validate' in job_map:
        pipeline_type = 'n'
    elif 'release-aarch64-darwin-release' in job_map:
        pipeline_type = 'r'
    else:
        pipeline_type = 'v'
    eprint(f"Pipeline Type: {pipeline_type}")

    job_url = artifact_url(job_map, "lint-ci-config", ".gitlab/jobs-metadata.json")
    eprint(job_url)
    import urllib.request
    urllib.request.urlretrieve(job_url, "jobs-metadata.json")
    metadata_file = "jobs-metadata.json"

    job_url = artifact_url(job_map, "project-version", "version.sh")
    eprint(job_url)
    import urllib.request
    urllib.request.urlretrieve(job_url, "version.sh")
    with open("version.sh", 'r') as f:
        version = re.search("[\d\.]+", f.read())[0]
    eprint(version)


    eprint(f"Reading job metadata from {metadata_file}.")
    with open(metadata_file, 'r') as f:
        job_mapping = json.load(f)

    eprint(f"Supported platforms: {job_mapping.keys()}")

    new_yaml = mk_new_yaml(job_map, job_mapping, pipeline_type, version)

    new_yaml['include'] = 'template.yml'

    print(yaml.dump(new_yaml))



if __name__ == '__main__':
    main()

