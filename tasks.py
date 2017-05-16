import os
import sys

from invoke import task, run


@task
def build_docs(ctx):
    """Builds html documentation and updates gh-pages branch.
    """
    def git(cmd):
        return run('cd docs/build/html && git {0}'.format(cmd))

    # build docs
    run('cd docs && make html')

    # If project's directory is git repository
    if os.path.exists('.git'):
        # if no git repository in docs/build/html,
        # then init one
        if not os.path.exists('docs/build/html/.git'):
            result = run("git remote -v | grep '^origin.*(push)$'", warn=True)

            if result.failed:
                print('There is no "origin" remote in this git repository.')
                print('Please, add remote and push it to the Github.')
                sys.exit(1)
            else:
                origin = result.stdout.strip().split()[1]
                git('init')
                git('remote add origin {0}'.format(origin))

        git('add .')
        git('commit -m "Update docs"')
        git('push --force origin master:gh-pages')
    else:
        # If project's directory is not a repository
        # then we don't know where to push the docs.
        print('This project is not a git repository.')
        print('Please, push it to the GitHub and run this command')
        print('again. Then we\'ll be able to update gh-pages branch.')
        sys.exit(1)
