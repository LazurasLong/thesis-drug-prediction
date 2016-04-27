### This script crawls the "offline" full antidepressant drugs of PubChem CID (80 drugs in total) ###

from __future__ import print_function
from bs4 import BeautifulSoup

with open('PubChem_antidepressant.html', 'r') as file:
    page_content = file.read()
    soup = BeautifulSoup(page_content, "lxml")

    result_list = []
    for element in soup.find_all("dl", class_ = "rprtid"):
        result_list.append(element.find("dt", class_ = "termtext").next_sibling.text.strip())

    print("c(", end = "")
    for item in result_list[:-1]:
        print(item + ", ", end = "")
    print(result_list[-1] + ")")
