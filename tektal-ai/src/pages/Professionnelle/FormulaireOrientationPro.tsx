import React, { useState, useEffect } from 'react';

export default function FormulaireOrientationPro() {
  const [formData, setFormData] = useState({
    nom: '',
    niveau_etude: '', // chaîne de caractères, pas un tableau
    filiere: '', // chaîne de caractères, pas un tableau
    competences: [], // tableau, multiple choix
    traits_personnalite: [] // tableau, multiple choix
  });
  const [step, setStep] = useState(1);
  const [error, setError] = useState('');
  const [competencesOptions, setCompetencesOptions] = useState([]);
  const [traitsOptions, setTraitsOptions] = useState([]);
  const [filieresOptions, setFilieresOptions] = useState([]);
  const [niveauxEtudeOptions, setNiveauxEtudeOptions] = useState([]);

  useEffect(() => {
    fetch('http://localhost:8000/competences')
      .then((res) => res.json())
      .then((data) => {
        if (data.competences && Array.isArray(data.competences)) {
          setCompetencesOptions(data.competences);
        } else {
          console.error("Données invalides pour compétences :", data);
          setCompetencesOptions([]);
        }
      })
      .catch((err) => console.error("Erreur lors de la récupération des compétences", err));

    fetch('http://localhost:8000/personnalites')
      .then((res) => res.json())
      .then((data) => {
        if (data.personnalites && Array.isArray(data.personnalites)) {
          setTraitsOptions(data.personnalites);
        } else {
          console.error("Données invalides pour personnalités :", data);
          setTraitsOptions([]);
        }
      })
      .catch((err) => console.error("Erreur lors de la récupération des personnalités", err));

    fetch('http://localhost:8000/filieres')
      .then((res) => res.json())
      .then((data) => {
        if (data.filieres && Array.isArray(data.filieres)) {
          setFilieresOptions(data.filieres);
        } else {
          console.error("Données invalides pour filières :", data);
          setFilieresOptions([]);
        }
      })
      .catch((err) => console.error("Erreur lors de la récupération des filières", err));

    fetch('http://localhost:8000/niveaux_etude')
      .then((res) => res.json())
      .then((data) => {
        if (data.niveaux_etude && Array.isArray(data.niveaux_etude)) {
          setNiveauxEtudeOptions(data.niveaux_etude);
        } else {
          console.error("Données invalides pour niveaux d'étude :", data);
          setNiveauxEtudeOptions([]);
        }
      })
      .catch((err) => console.error("Erreur lors de la récupération des niveaux d'étude", err));
  }, []);

  const handleCheckboxChange = (e, field) => {
    const { value, checked } = e.target;
    if (checked) {
      // Si on coche une case, on ajoute la valeur au tableau
      setFormData((prevData) => ({
        ...prevData,
        [field]: [...prevData[field], value]
      }));
    } else {
      // Si on décoche une case, on retire la valeur du tableau
      setFormData((prevData) => ({
        ...prevData,
        [field]: prevData[field].filter((item) => item !== value)
      }));
    }
  };

  const handleSingleChoiceChange = (e, field) => {
    const { value } = e.target;
    setFormData((prevData) => ({
      ...prevData,
      [field]: value // On enregistre la valeur comme chaîne, pas un tableau
    }));
  };

  const handleNext = () => {
    if (step < 5) setStep(step + 1);
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    
    try {
      const response = await fetch('http://localhost:8000/orientation_professionnelle', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(formData)
      });
      
      if (!response.ok) {
        throw new Error("Erreur lors de l'envoi des données");
      }
      
      console.log('Données envoyées avec succès');
    } catch (error) {
      setError(error.message);
    }
  };

  return (
    <div className="max-w-lg mx-auto p-4 border rounded shadow">
      <h2 className="text-xl font-bold mb-4">Formulaire d'orientation</h2>
      {error && <p className="text-red-500">{error}</p>}

      {step === 1 && (
        <input
          type="text"
          name="nom"
          placeholder="Nom"
          value={formData.nom}
          onChange={(e) => setFormData({ ...formData, nom: e.target.value })}
          className="w-full p-2 border rounded mb-2"
        />
      )}

      {step === 2 && (
        <div>
          <h3 className="font-semibold mb-2">Niveau d'étude :</h3>
          {niveauxEtudeOptions.map((niveau) => (
            <label key={niveau} className="block mb-1">
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
          <h3 className="font-semibold mb-2">Filière :</h3>
          {filieresOptions.map((filiere) => (
            <label key={filiere} className="block mb-1">
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
          <h3 className="font-semibold mb-2">Compétences :</h3>
          {competencesOptions.map((comp) => (
            <label key={comp} className="block mb-1">
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
          <h3 className="font-semibold mb-2">Traits de personnalité :</h3>
          {traitsOptions.map((trait) => (
            <label key={trait} className="block mb-1">
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

      <div className="mt-4 flex justify-between">
        {step < 5 ? (
          <button onClick={handleNext} className="px-4 py-2 bg-green-600 text-white rounded-md">Suivant</button>
        ) : (
          <button onClick={handleSubmit} className="px-4 py-2 bg-blue-600 text-white rounded-md">Soumettre</button>
        )}
      </div>
    </div>
  );
}
