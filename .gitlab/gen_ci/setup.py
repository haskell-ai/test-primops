#!/usr/bin/env python

from distutils.core import setup

setup(name='gen_ci',
      author='Matthew Pickering',
      author_email='matthew@well-typed.com',
      py_modules=['gen_ci'],
      entry_points={
          'console_scripts': [
              'gen_ci=gen_ci:main',
          ]
      }
     )
