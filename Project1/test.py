import re

test = "+= /= -= *=|| != >= &&'h' # The next line intentionally left blank!;"


print(re.search("\#[.]*", test))
