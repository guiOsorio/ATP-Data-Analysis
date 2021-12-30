import pandas as pd
import numpy as np
import sys
import time
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager


from selenium.webdriver.chrome.options import Options

# Scrape data for player table (id, ranking, country/region, name, age, height, weight, dominant hand)

##### TASK 1 - scrape data from https://www.atptour.com/en/rankings/singles for -> ranking, country, name, age, height, weight, dominant hand -- for each player

url_player = "https://www.atptour.com/en/rankings/singles"
driver = webdriver.Chrome(ChromeDriverManager().install())
driver.get(url_player)
time.sleep(3)
page_players = driver.page_source
driver.quit()

players = BeautifulSoup(page_players, "html.parser")
players_player = players.select_one(".mega-table").select_one("tbody").find_all(recursive=False)

player_info = []
for player in players_player:
    rank = player.select_one(".rank-cell").get_text().strip()
    country = player.select_one(".country-item").select_one("img")["alt"]
    if country == "GER":
        country = "DEU"
    elif country == "GRE":
        country = "GRC"
    elif country == "SUI":
        country = "CHE"
    elif country == "CHI":
        country = "CHL"
    elif country == "BUL":
        country = "BGR"
    elif country == "CRO":
        country = "HRV"
    elif country == "RSA":
        country = "ZAF"
    elif country == "NED":
        country = "NLD"
    elif country == "URU":
        country = "URY"
    name = player.select_one(".player-cell").select_one("a").get_text().strip()
    age = player.select_one(".age-cell").get_text().strip()
    player_link = player.select_one(".player-cell").select_one("a")["href"]

    url = "https://www.atptour.com/" + player_link
    driver = webdriver.Chrome(ChromeDriverManager().install())
    driver.get(url)
    time.sleep(3)
    page_player = driver.page_source
    driver.quit()

    def uprint(*objects, sep=' ', end='\n', file=sys.stdout):
        enc = file.encoding
        if enc == 'UTF-8':
            print(*objects, sep=sep, end=end, file=file)
        else:
            f = lambda obj: str(obj).encode(enc, errors='backslashreplace').decode(enc)
            print(*map(f, objects), sep=sep, end=end, file=file)
    
    player_ind = BeautifulSoup(page_player, "html.parser")
    weight_lbs = int(player_ind.select_one(".table-weight-lbs").get_text())
    height_cms = int(player_ind.select_one(".table-height-cm-wrapper").get_text()[1:4])

    new_player = {"Rank": rank, "Region": country, "Name": name, "Age": age, "Weight(lbs)": weight_lbs, "Height(cms)": height_cms}
    player_info.append(new_player)

# ##### TASK 2 - scrape data from https://www.iban.com/country-codes and change the country's code to be the actual country's full name

# Request URL
country_codes_req = requests.get("https://www.iban.com/country-codes")

# Initialize BS
country_codes_page = BeautifulSoup(country_codes_req.text, "html.parser")
country_codes = country_codes_page.select_one("#myTable").select_one("tbody").find_all(recursive=False)

countries = {}
for country in country_codes:
    country_full = country.select("td")[0].get_text()
    country_3code = country.select("td")[2].get_text()
    countries[country_3code] = country_full

for player in player_info:
    player["Region"] = countries[player["Region"]]
    if player["Region"] == "Netherlands (the)":
        player["Region"] = "Netherlands"
    elif player["Region"] == "Russian Federation (the)":
        player["Region"] = "Russian Federation"
    elif player["Region"] == "United Kingdom of Great Britain and Northern Ireland (the)":
        player["Region"] = "United Kingdom of Great Britain and Northern Island"
    elif player["Region"] == "United States of America (the)":
        player["Region"] = "United States of America"
    elif player["Region"] == "Korea (the Republic of)":
        player["Region"] = "Korea, Republic of"

##### TASK 3 - scrape data from https://help.adjust.com/en/article/countries-by-region and change the country's name to be the region's code

# Request URL
regions_req = requests.get("https://help.adjust.com/en/article/countries-by-region")

# Initialize BS
regions_page = BeautifulSoup(regions_req.text, "html.parser")
regions_tags = regions_page.select_one(".thc_table").select_one(".inline-block").select_one(".inline-block").select_one("tbody").find_all(recursive=False)

regions = {}
for region in regions_tags:
    country_name = region.select("td")[1].get_text()
    country_region = region.select("td")[2].get_text()
    regions[country_name] = country_region

for player in player_info:
    player["Region"] = regions[player["Region"]]

##### TASK 4 - send final data to a csv file

players_final = []
for player in player_info:
    new_player = (player["Rank"], player["Region"], player["Name"], player["Age"], player["Weight(lbs)"], player["Height(cms)"])
    players_final.append(new_player)

print(players_final)

player_col_names = ['Rank', 'Region', 'Name', 'Age', 'Weight(lbs)', 'Height(cms)']
player_data = np.array(players_final)
player_df = pd.DataFrame(player_data, columns=player_col_names)
player_df.to_csv('player_info.csv')