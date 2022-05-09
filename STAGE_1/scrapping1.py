import pandas as pd
import numpy as np
import time
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

# Scrape data for serve leaders, return leaders and under pressure leaders
# Add country and continent/region, age, height, and end of the year 2021 ranking of each player

#### SERVE

url_serve = "https://www.atptour.com/en/stats/leaderboard?boardType=serve&timeFrame=52Week&surface=all&versusRank=all&formerNo1=false"
options = Options()
options.add_argument('--headless')
options.add_argument('--disable-gpu')
driver = webdriver.Chrome(options=options, executable_path=r"C:\Users\Guilh\Documents\Code\ATP\ChromeDriver\chromedriver.exe")
driver.get(url_serve)
time.sleep(3)
page_serve = driver.page_source
driver.quit()

serve = BeautifulSoup(page_serve, "html.parser")
players_serve = serve.find_all('tr', class_='stats-listing-row')

serve_stats = []
for player in players_serve:
    rank = int(player.select("td")[0].get_text())
    name = player.select_one(".leaderboard-info").select_one(".player-leaderboard-info").select_one(".stats-player-name").get_text()
    serve_rating = float(player.select("td")[2].get_text())
    first_perc = round(float(player.select("td")[3].get_text()[0:4])/100, 3)
    first_won_perc = round(float(player.select("td")[4].get_text()[0:4])/100, 3)
    second_won_perc = round(float(player.select("td")[5].get_text()[0:4])/100, 3)
    games_won_perc = round(float(player.select("td")[6].get_text()[0:4])/100, 3)
    aces_match = float(player.select("td")[7].get_text())
    dfs_match = float(player.select("td")[8].get_text())
    new_player = (rank, name, serve_rating, first_perc, first_won_perc, second_won_perc, games_won_perc, aces_match, dfs_match)
    serve_stats.append(new_player)

print(serve_stats)

# serve_col_names = ['Rank', 'Name', 'Rating', 'First_perc', 'First_won_perc', 'Second_won_perc', 'Games_won_perc', 'Aces_match', 'Dfs_match']
# serve_data = np.array(serve_stats)
# serve_df = pd.DataFrame(serve_data, columns=serve_col_names)
# serve_df.to_csv('serve_stats.csv')

# ####

# #### RETURN

# url_return = "https://www.atptour.com/en/stats/leaderboard?boardType=return&timeFrame=52Week&surface=all&versusRank=all&formerNo1=false"
# options = Options()
# options.add_argument('--headless')
# options.add_argument('--disable-gpu')
# driver = webdriver.Chrome(options=options, executable_path=r"C:\Users\Guilh\Documents\Code\ATP\ChromeDriver\chromedriver.exe")
# driver.get(url_return)
# time.sleep(3)
# page_return = driver.page_source
# driver.quit()

# returns = BeautifulSoup(page_return, "html.parser")
# players_return = returns.find_all('tr', class_='stats-listing-row')

# return_stats = []
# for player in players_return:
#     rank = int(player.select("td")[0].get_text())
#     name = player.select_one(".leaderboard-info").select_one(".player-leaderboard-info").select_one(".stats-player-name").get_text()
#     return_rating = float(player.select("td")[2].get_text())
#     first_won_perc = round(float(player.select("td")[3].get_text()[0:4])/100, 3)
#     second_won_perc = round(float(player.select("td")[4].get_text()[0:4])/100, 3)
#     games_won_perc = round(float(player.select("td")[5].get_text()[0:3])/100, 3)
#     bp_converted_perc = round(float(player.select("td")[6].get_text()[0:4])/100, 3)
#     new_player = (rank, name, return_rating, first_won_perc, second_won_perc, games_won_perc, bp_converted_perc)
#     return_stats.append(new_player)

# return_col_names = ['Rank', 'Name', 'Rating', 'First_won_perc', 'Second_won_perc', 'Games_won_perc', 'BPs_converted_perc']
# return_data = np.array(return_stats)
# return_df = pd.DataFrame(return_data, columns=return_col_names)
# return_df.to_csv('return_stats.csv')

# ####

# #### UNDER PRESSURE
# url_pressure = "https://www.atptour.com/en/stats/leaderboard?boardType=pressure&timeFrame=52Week&surface=all&versusRank=all&formerNo1=false"
# options = Options()
# options.add_argument('--headless')
# options.add_argument('--disable-gpu')
# driver = webdriver.Chrome(options=options, executable_path=r"C:\Users\Guilh\Documents\Code\ATP\ChromeDriver\chromedriver.exe")
# driver.get(url_pressure)
# time.sleep(3)
# page_pressure = driver.page_source
# driver.quit()

# pressure = BeautifulSoup(page_pressure, "html.parser")
# players_pressure = pressure.find_all('tr', class_='stats-listing-row')

# pressure_stats = []
# for player in players_pressure:
#     rank = int(player.select("td")[0].get_text())
#     name = player.select_one(".leaderboard-info").select_one(".player-leaderboard-info").select_one(".stats-player-name").get_text()
#     pressure_rating = float(player.select("td")[2].get_text())
#     bps_won_perc = round(float(player.select("td")[3].get_text()[0:4])/100, 3)
#     bps_saved_perc = round(float(player.select("td")[4].get_text()[0:4])/100, 3)
#     tiebreaks_won_perc = round(float(player.select("td")[5].get_text()[0:4])/100, 3)
#     deciding_set_won_perc = round(float(player.select("td")[6].get_text()[0:4])/100, 3)
#     new_player = (rank, name, pressure_rating, bps_won_perc, bps_saved_perc, tiebreaks_won_perc, deciding_set_won_perc)
#     pressure_stats.append(new_player)

# pressure_col_names = ['Rank', 'Name', 'Rating', 'BPs_won_perc', 'BPs_saved_perc', 'Tiebreaks_won_perc', 'Deciding_set_won_perc']
# pressure_data = np.array(pressure_stats)
# pressure_df = pd.DataFrame(pressure_data, columns=pressure_col_names)
# pressure_df.to_csv('pressure_stats.csv')

# ####
