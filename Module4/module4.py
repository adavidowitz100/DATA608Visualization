from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import pandas as pd
import dash_bootstrap_components as dbc
from dash_bootstrap_templates import load_figure_template

df = pd.read_csv('https://raw.githubusercontent.com/adavidowitz100/DATA608Visualization/main/Module4/2015_Street_Tree_Census_-_Tree_Data_subset.csv')
#df = pd.read_csv('2015_Street_Tree_Census_-_Tree_Data_subset.csv')

app = Dash(external_stylesheets=[dbc.themes.MINTY])

load_figure_template('MINTY')


app.layout = html.Div([
    html.H1(children='New York City Tree Health', style={'textAlign':'center'}),
    html.Div([
        html.Div(children=[
            html.Label("Borough:"),
            dcc.Dropdown(df.borough.unique(), 'Queens', id='borough-selection')
            ]),
        html.Div(children=[
            html.Label("Tree Species:"),
            dcc.Dropdown(df.spc_common.unique(), 'red maple', id='species-selection')
            ])
    ], style ={ 'width': '25%'} ),
    html.Div(children=[
        dcc.Graph(figure={}, id='graph-health-steward')
    ]
    )
])

@callback(
    Output('graph-health-steward', 'figure'),
    Input('borough-selection', 'value'),
    Input('species-selection', 'value')
)
def update_graph(borough_selection, species_selection):
    dff = df[(df.borough==borough_selection) & (df.spc_common==species_selection)]
    fig1 = px.histogram(dff, x='health', color='steward', category_orders=dict(health=["Poor", "Fair", "Good"]), 
                        text_auto = True, title='Steward Makeup of Tree Health by Borough & Species')
    return fig1

if __name__ == '__main__':
    app.run_server(debug=True)