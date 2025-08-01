
import streamlit as st
import pandas as pd

# App Title
st.set_page_config(page_title="Music Suggestion App", page_icon="🎵")
st.title("🎵 Music Suggestion App")
st.markdown("Select your **mood** and **preferred language** to get song recommendations 🎶")

# Dummy dataset
songs = {
    'Title': [
        'Happy - Pharrell Williams', 'Someone Like You - Adele', 'Blinding Lights - The Weeknd',
        'Weightless - Marconi Union', 'Believer - Imagine Dragons', 'Perfect - Ed Sheeran',
        'Tum Hi Ho - Arijit Singh', 'Why This Kolaveri Di', 'Vaathi Coming', 'Kun Faya Kun'
    ],
    'Mood': ['Happy', 'Sad', 'Energetic', 'Relaxed', 'Energetic', 'Romantic', 'Sad', 'Funny', 'Energetic', 'Relaxed'],
    'Language': ['English', 'English', 'English', 'English', 'English', 'English', 'Hindi', 'Tamil', 'Tamil', 'Hindi']
}

df = pd.DataFrame(songs)

# Sidebar filters
st.sidebar.header("🎯 Choose Filters")
mood = st.sidebar.selectbox("What's your mood?", df['Mood'].unique())
language = st.sidebar.selectbox("Preferred language?", ['Any'] + sorted(df['Language'].unique()))

# Filter songs
if language == 'Any':
    suggestions = df[df['Mood'] == mood]
else:
    suggestions = df[(df['Mood'] == mood) & (df['Language'] == language)]

# Display results
st.subheader("🎧 Recommended Songs:")
if not suggestions.empty:
    for i, row in suggestions.iterrows():
        st.write(f"- **{row['Title']}** *(Language: {row['Language']})*")
else:
    st.warning("😢 No songs found for your mood and language preference.")
