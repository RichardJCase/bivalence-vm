#!/usr/bin/python

import os

open("test.b", "w").write("\0" * int(os.environ["PAGE_SIZE"]) * (int(os.environ["NUM_PAGE"]) + 1))
