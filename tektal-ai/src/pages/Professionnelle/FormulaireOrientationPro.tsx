import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';

export default function FormulaireOrientationPro() {
  const [formData, setFormData] = useState({
    nom: '',
    niveau_etude: '', // chaîne de caractères, pas un tableau
    filiere: '', // chaîne de caractères, pas un tableau
    competences: [], // tableau, multiple choix
    traits_personnalite: [], // tableau, multiple choix
  });
  const [step, setStep] = useState(1);
  const [error, setError] = useState('');
  const [competencesOptions, setCompetencesOptions] = useState([]);
  const [traitsOptions, setTraitsOptions] = useState([]);
  const [filieresOptions, setFilieresOptions] = useState([]);
  const [niveauxEtudeOptions, setNiveauxEtudeOptions] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const navigate = useNavigate();

  useEffect(() => {
    // Récupérer les options depuis l'API
    const fetchData = async () => {
      try {
        const competencesResponse = await fetch('http://localhost:8000/competences');
        const competencesData = await competencesResponse.json();
        if (competencesData.competences && Array.isArray(competencesData.competences)) {
          setCompetencesOptions(competencesData.competences);
        } else {
          console.error("Données invalides pour compétences :", competencesData);
        }

        const traitsResponse = await fetch('http://localhost:8000/personnalites');
        const traitsData = await traitsResponse.json();
        if (traitsData.personnalites && Array.isArray(traitsData.personnalites)) {
          setTraitsOptions(traitsData.personnalites);
        } else {
          console.error("Données invalides pour personnalités :", traitsData);
        }

        const filieresResponse = await fetch('http://localhost:8000/filieres');
        const filieresData = await filieresResponse.json();
        if (filieresData.filieres && Array.isArray(filieresData.filieres)) {
          setFilieresOptions(filieresData.filieres);
        } else {
          console.error("Données invalides pour filières :", filieresData);
        }

        const niveauxResponse = await fetch('http://localhost:8000/niveaux_etude');
        const niveauxData = await niveauxResponse.json();
        if (niveauxData.niveaux_etude && Array.isArray(niveauxData.niveaux_etude)) {
          setNiveauxEtudeOptions(niveauxData.niveaux_etude);
        } else {
          console.error("Données invalides pour niveaux d'étude :", niveauxData);
        }
      } catch (err) {
        console.error("Erreur lors de la récupération des données :", err);
      }
    };

    fetchData();
  }, []);

  const handleCheckboxChange = (e, field) => {
    const { value, checked } = e.target;
    if (checked) {
      setFormData((prevData) => ({
        ...prevData,
        [field]: [...prevData[field], value],
      }));
    } else {
      setFormData((prevData) => ({
        ...prevData,
        [field]: prevData[field].filter((item) => item !== value),
      }));
    }
  };

  const handleSingleChoiceChange = (e, field) => {
    const { value } = e.target;
    setFormData((prevData) => ({
      ...prevData,
      [field]: value,
    }));
  };

  const handleNext = () => {
    if (step < 5) setStep(step + 1);
  };

  const handlePrevious = () => {
    if (step > 1) setStep(step - 1);
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    setError('');
  
    try {
      const response = await fetch('http://localhost:8000/orientation_professionnelle', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(formData),
      });
  
      if (!response.ok) {
        throw new Error("Erreur lors de l'envoi des données");
      }
  
      const result = await response.json();
      console.log('Réponse du serveur :', result);
  
      // Redirection vers la page de résultats avec les données
      navigate('/professionnel-resultats', {
        state: {
          resultats: result, // Réponse du serveur
          utilisateurData: formData, // Données de l'utilisateur
        },
      });
    } catch (err) {
      setError(err.message);
      console.error('Erreur :', err);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="flex justify-center items-center min-h-screen bgCMC">
      <div className="w-full max-w-3xl mx-auto p-6 bg-white rounded-lg shadow-lg">
        <h2 className="text-2xl font-bold text-center mb-6">Orientation Professionnelle avec TEKTAL-Ai !</h2>
        {error && <p className="text-red-500 text-center mb-4">{error}</p>}

        {/* Barre de progression */}
        <div className="w-full bg-gray-200 rounded-full h-2.5 mb-6">
          <div className="bg-yellow-400 h-2.5 rounded-full" style={{ width: `${(step / 5) * 100}%` }}></div>
        </div>

        {/* Étapes du formulaire */}
        {!isLoading ? (
          <div className="space-y-4">
            {step === 1 && (
              <div>
                <h2 className="text-xl font-semibold mb-4">📜 Informations Personnelles</h2>
                <label className="block text-sm font-medium text-gray-700">➪ Quel est votre nom complet ?</label>
                <input
                  type="text"
                  name="nom"
                  placeholder="Entrez votre nom"
                  value={formData.nom}
                  onChange={(e) => setFormData({ ...formData, nom: e.target.value })}
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                />
              </div>
            )}

            {step === 2 && (
              <div>
                <h3 className="text-xl font-semibold mb-4">🎓 Niveau d'étude</h3>
                {niveauxEtudeOptions.map((niveau) => (
                  <label key={niveau} className="block mb-2">
                    <input
                      type="radio"
                      value={niveau}
                      checked={formData.niveau_etude === niveau}
                      onChange={(e) => handleSingleChoiceChange(e, 'niveau_etude')}
                      className="mr-2"
                    />
                    {niveau}
                  </label>
                ))}
              </div>
            )}

            {step === 3 && (
              <div>
                <h3 className="text-xl font-semibold mb-4">📚 Filière</h3>
                {filieresOptions.map((filiere) => (
                  <label key={filiere} className="block mb-2">
                    <input
                      type="radio"
                      value={filiere}
                      checked={formData.filiere === filiere}
                      onChange={(e) => handleSingleChoiceChange(e, 'filiere')}
                      className="mr-2"
                    />
                    {filiere}
                  </label>
                ))}
              </div>
            )}

            {step === 4 && (
              <div>
                <h3 className="text-xl font-semibold mb-4">🛠️ Compétences</h3>
                {competencesOptions.map((comp) => (
                  <label key={comp} className="block mb-2">
                    <input
                      type="checkbox"
                      value={comp}
                      checked={formData.competences.includes(comp)}
                      onChange={(e) => handleCheckboxChange(e, 'competences')}
                      className="mr-2"
                    />
                    {comp}
                  </label>
                ))}
              </div>
            )}

            {step === 5 && (
              <div>
                <h3 className="text-xl font-semibold mb-4">🧠 Traits de personnalité</h3>
                {traitsOptions.map((trait) => (
                  <label key={trait} className="block mb-2">
                    <input
                      type="checkbox"
                      value={trait}
                      checked={formData.traits_personnalite.includes(trait)}
                      onChange={(e) => handleCheckboxChange(e, 'traits_personnalite')}
                      className="mr-2"
                    />
                    {trait}
                  </label>
                ))}
              </div>
            )}
          </div>
        ) : (
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-500 mx-auto mb-4"></div>
            <p className="text-gray-700">Traitement en cours...</p>
          </div>
        )}

        {/* Boutons de navigation */}
        {!isLoading && (
          <div className="mt-6 flex justify-between">
            {step > 1 && (
              <button
                onClick={handlePrevious}
                className="px-4 py-2 bg-gray-500 text-white rounded-md hover:bg-gray-600"
              >
                👈 Précédent
              </button>
            )}
            {step < 5 ? (
              <button
                onClick={handleNext}
                className="px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700"
              >
                Suivant 👉
              </button>
            ) : (
              <button
                onClick={handleSubmit}
                className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700"
              >
                Soumettre
              </button>
            )}
          </div>
        )}
      </div>
    </div>
  );
}