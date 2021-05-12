### DASH APP FOR STAT 430

import dash
from dash import Dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

import pandas as pd
import numpy as np

import plotly.express as px
from plotly.tools import FigureFactory as ff

###

df = pd.read_csv('/Users/lucasnelson/Desktop/University of Illinois/Junior/SP21/STAT 430/final_proj/statcast_pitcher.csv')

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

all_options = {
    '2016RHP': [player_name for player_name in df.loc[(df['game_year'] == 2016) & (df['p_throws'] == 'R'), 'player_name'].unique()],
    '2016LHP': [player_name for player_name in df.loc[(df['game_year'] == 2016) & (df['p_throws'] == 'L'), 'player_name'].unique()],
    '2017RHP': [player_name for player_name in df.loc[(df['game_year'] == 2017) & (df['p_throws'] == 'R'), 'player_name'].unique()],
    '2017LHP': [player_name for player_name in df.loc[(df['game_year'] == 2017) & (df['p_throws'] == 'L'), 'player_name'].unique()],
    '2018RHP': [player_name for player_name in df.loc[(df['game_year'] == 2018) & (df['p_throws'] == 'R'), 'player_name'].unique()],
    '2018LHP': [player_name for player_name in df.loc[(df['game_year'] == 2018) & (df['p_throws'] == 'L'), 'player_name'].unique()],
    '2019RHP': [player_name for player_name in df.loc[(df['game_year'] == 2019) & (df['p_throws'] == 'R'), 'player_name'].unique()],
    '2019LHP': [player_name for player_name in df.loc[(df['game_year'] == 2019) & (df['p_throws'] == 'L'), 'player_name'].unique()],
    '2020RHP': [player_name for player_name in df.loc[(df['game_year'] == 2020) & (df['p_throws'] == 'R'), 'player_name'].unique()],
    '2020LHP': [player_name for player_name in df.loc[(df['game_year'] == 2020) & (df['p_throws'] == 'L'), 'player_name'].unique()],
}

def generate_table(dataframe, max_rows=30000):
    return html.Table([
        html.Thead(
            html.Tr([html.Th(col) for col in dataframe.columns if col != 'Unnamed: 0'])
        ),
        html.Tbody([
            html.Tr([
                html.Td(dataframe.iloc[i][col]) for col in dataframe.columns  if col != 'Unnamed: 0'
            ]) for i in range(0, min(len(dataframe), max_rows), 3000)
        ])
    ], style={'marginLeft': 'auto', 'marginRight': 'auto'})

###

app.layout = html.Div(children=[
    html.H1(
        html.Strong(children='Statcast Pitcher Velocity Distribution'),
        style={'textAlign': 'center'}
    ),

    html.Div(children='This Dash app features the ability for users to visualize the velocity distribution per season-pitcher pairing. To visualize a pitcher\'s velocity distribution, use the slider to select the desired season (ranging 2016 to 2020) and either dropdown menu to select a picther (RHP or LHP). The menus will update as inputs are provided in the other fields to ensure that all user inputs are valid.',
    style={'textAlign': 'center'}),

    html.Hr(),

    html.H4(html.Strong(children='Parameters for Pitcher Velocity Distribution Visualization'),
        style={'textAlign' : 'center'}),
    
    html.H5(html.Strong('Regular Season')),
    html.P('Select a season that you would like to observe. The dropdown menu will filter to include pitchers from only this season.'),
    
    dcc.Slider(
        id='year-slider',
        min=df['game_year'].min(),
        max=df['game_year'].max(),
        value=df['game_year'].min() + 1,
        marks={str(year): str(year) for year in df['game_year'].unique()},
        step=None
    ),

    html.H5(html.Strong('Pitcher\'s Throwing Hand')),
    html.P('Select a throwing hand that you would like to observe. The dropdown menu will filter to include pitchers who only throw with this hand.'),

    dcc.RadioItems(
        id='hand-radio',
        options=[{'label': k, 'value': k} for k in ['RHP', 'LHP']],
        value='RHP'
    ),
    
    html.H5(html.Strong('Pitcher')),
    html.P('Select a pitcher that you would like to observe. The graph below will display the pitcher\'s velocity distribution by pitch type for the selected season.'),

    dcc.Dropdown(id='pitcher-drop'),

    dcc.Graph(id='pitcher-release-speed'),

    dcc.Graph(id='pitcher-release-spin'),

    html.Hr(),

    html.H4(html.Strong(children='Statcast Pitcher DataFrame'),
            style={'textAlign': 'center'}),
    html.P(children='Preview of .csv file utilized for app. Selected unique pitcher IDs from statcast data to append individual pitcher data frames to the data set previewed below.',
            style={'textAlign': 'center'}),
    html.P(children='Contains data per pitcher-pitch pairing for entire 2016, 2017, 2018, 2019, and 2020 seasons. Season data stored in [game_year]. Pitcher data stored in [pitch_name, p_throws]. Pitch data stored in [pitch_type, pitch_name, release_speed, release_spin_rate].',
            style={'textAlign': 'center'}),
    generate_table(df)
])

###

@app.callback(
    Output('pitcher-drop', 'options'),
    Input('year-slider', 'value'),
    Input('hand-radio', 'value'))
def set_pitcher_options(selected_year, selected_hand):
    return [{'label': i, 'value': i} for i in all_options[str(selected_year) + str(selected_hand)]]


@app.callback(
    Output('pitcher-drop', 'value'),
    Input('pitcher-drop', 'options'))
def get_pitcher_value(pitcher_options):
    return pitcher_options[0]['value']


@app.callback(
    Output('pitcher-release-speed', 'figure'),
    Input('pitcher-drop', 'value'),
    Input('year-slider', 'value'),
    Input('hand-radio', 'value'))
def set_display_speed(pitcher_name, selected_year, selected_hand):
    pitcher_df = df[(df['game_year'] == selected_year) & (df['p_throws'] == selected_hand[0]) & (df['player_name'] == pitcher_name)].dropna()
    
    kde_data = []
    pitch_labels = []
    for pitch_type in pitcher_df['pitch_type'].unique():
        if (pitch_type != 'PO') & (pitch_type != 'IN'):
            kde_data.append(pitcher_df[pitcher_df['pitch_type'] == pitch_type]['release_speed'])
            pitch_labels.append(pitch_type)
        else:
            continue

    fig = ff.create_distplot(kde_data, pitch_labels, show_hist=False, show_rug=False)
    fig.update_layout(transition_duration=500,
                    title='{}: {} Pitch Velocity Distribution'.format(pitcher_name[pitcher_name.rfind(', ') + 2:] + ' ' + pitcher_name[:pitcher_name.find(',')], selected_year),
                    xaxis_title='Miles Per Hour (mph)',
                    yaxis_title='Frequency of Speed (%)',
                    title_x=0.5)

    return fig


@app.callback(
    Output('pitcher-release-spin', 'figure'),
    Input('pitcher-drop', 'value'),
    Input('year-slider', 'value'),
    Input('hand-radio', 'value'))
def set_display_spin(pitcher_name, selected_year, selected_hand):
    pitcher_df = df[(df['game_year'] == selected_year) & (df['p_throws'] == selected_hand[0]) & (df['player_name'] == pitcher_name)].dropna()
    
    kde_data = []
    pitch_labels = []
    for pitch_type in pitcher_df['pitch_type'].unique():
        if (pitch_type != 'PO') & (pitch_type != 'IN'):
            kde_data.append(pitcher_df[pitcher_df['pitch_type'] == pitch_type]['release_spin_rate'])
            pitch_labels.append(pitch_type)
        else:
            continue

    fig = ff.create_distplot(kde_data, pitch_labels, show_hist=False, show_rug=False)
    fig.update_layout(transition_duration=500,
                    title='{}: {} Spin Rate Distribution'.format(pitcher_name[pitcher_name.rfind(', ') + 2:] + ' ' + pitcher_name[:pitcher_name.find(',')], selected_year),
                    xaxis_title='Revolutions per Minute (rpm)',
                    yaxis_title='Frequency of Rate (%)',
                    title_x=0.5)

    return fig

###

if __name__ == '__main__':
    app.run_server(debug=True)