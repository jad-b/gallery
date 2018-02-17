import os
from setuptools import setup, find_packages


def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()


setup(
    name="gallery",
    version="0.1.0",
    author="Jeremy Dobbins-Bucklad",
    author_email="j.american.db@gmail.com",
    description="Collection of algorithm & data structure implementations",
    entry_points={
        "console_scripts": [
            "pending=bin.pending:main"
        ]
    },
    license="MPLv2",
    keywords="algorithms data-structures",
    url="https://github.com/gallery",
    packages=find_packages(),
    long_description=read('README.md'),
    classifiers=[],
)
