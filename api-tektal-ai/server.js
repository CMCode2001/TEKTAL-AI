const express = require('express');
const app = express();
const port = 3030;

// Route de base
app.get('/', (req, res) => {
  res.send('API-TEKTAL');
});

// Démarrer le serveur
app.listen(port, () => {
  console.log(`Serveur démarré sur http://localhost:${port}`);
});