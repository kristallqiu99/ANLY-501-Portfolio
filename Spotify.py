'''import requests

CLIENT_ID = '6ced8cf12e0c470f9d9dd25a75d4ec26'
CLIENT_SECRET = '4ce366cebd7c42f2ac59bf96ca9f32b9'

AUTH_URL = 'https://accounts.spotify.com/api/token'

# POST
auth_response = requests.post(AUTH_URL, {
    'grant_type': 'client_credentials',
    'client_id': CLIENT_ID,
    'client_secret': CLIENT_SECRET,
})

# convert the response to JSON
auth_response_data = auth_response.json()

# save the access token
access_token = auth_response_data['access_token']

headers = {
    'Authorization': 'Bearer {token}'.format(token=access_token)
}

# base URL of all Spotify API endpoints
BASE_URL = 'https://api.spotify.com/v1/'

# Track ID from the URI
track_id = '2bgTY4UwhfBYhGT4HUYStN'

# actual GET request with proper header
r = requests.get(BASE_URL + 'audio-features/' + track_id, headers=headers)
print(r.json())



'''
# get track id by name and artist
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import json
cid = '6ced8cf12e0c470f9d9dd25a75d4ec26'
secret = '4ce366cebd7c42f2ac59bf96ca9f32b9'
client_credentials_manager = SpotifyClientCredentials(client_id=cid, client_secret=secret)
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

track_results = sp.search(q='year:2020', type='track')
print(track_results['tracks']['items'][0])

#print(sp.audio_features([track_id])[0])
#print(track_id)


