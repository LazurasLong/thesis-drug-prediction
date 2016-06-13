### This script crawls the first page of PubChem antidepressant CID. (20 drugs/page) ###
import scrapy
from scrapy.selector import Selector

class PubChemSpider(scrapy.Spider):
    name = "PubChem"
    start_urls = [
        "https://www.ncbi.nlm.nih.gov/pccompound/?term=antidepressant"
    ]

    def parse(self, response):
        hxs = Selector(response)
        print hxs.xpath('//dt[text()="CID: "]/following-sibling::dd/text()').extract()
