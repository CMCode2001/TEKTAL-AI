import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';

const FormulaireUtilisateur = () => {
  const [formData, setFormData] = useState({
    nom: '',
    niveau_etude: '',
    filiere: '',
    domaine_interet: '',
    notes: {},
  });

  const [idUtilisateur, setIdUtilisateur] = useState<number | null>(null);
  const [matRequis, setMatRequis] = useState<string[]>([]);
  const [resultatsAnalyse, setResultatsAnalyse] = useState(null);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [step, setStep] = useState(1);
  const navigate = useNavigate();

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target;
    setFormData({ ...formData, [name]: value });
  };

  const handleGradeChange = (subject: string, value: string) => {
    setFormData((prev) => ({
      ...prev,
      notes: {
        ...prev.notes,
        [subject]: Math.min(20, Math.max(0, Number(value) || 0)),
      },
    }));
  };

  const handleNext = () => {
    setStep(step + 1);
  };

  const handleSubmit = async () => {
    setIsAnalyzing(true);
    setError(null);

    try {
      const response = await fetch('http://localhost:8000/ajouter_utilisateur', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          nom: formData.nom,
          niveau_etude: formData.niveau_etude,
          filiere: formData.filiere,
          domaine_interet: formData.domaine_interet,
        }),
      });

      if (!response.ok) throw new Error("Erreur lors de l'ajout de l'utilisateur");

      const data = await response.json();
      setIdUtilisateur(data.id_utilisateur);
      setMatRequis(data.resultat);

      alert("Veuillez entrer vos notes pour les matières requises.");
    } catch (err) {
      setError((err as Error).message);
    } finally {
      setIsAnalyzing(false);
    }
  };

  const envoyerNotes = async () => {
    if (!idUtilisateur) {
      setError("L'utilisateur n'a pas encore été enregistré.");
      return;
    }

    // Vérification que toutes les notes sont bien renseignées
    if (Object.keys(formData.notes).length !== matRequis.length) {
      setError("Veuillez entrer une note pour chaque matière.");
      return;
    }

    setIsAnalyzing(true);
    setError(null);

    try {
      const requestBody = {
        id_utilisateur: idUtilisateur,
        notes: formData.notes,
      };

      const notesResponse = await fetch('http://localhost:8000/traiter_notes_utilisateur', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(requestBody),
      });

      if (!notesResponse.ok) throw new Error("Erreur lors du traitement des notes");

      const resultats = await notesResponse.json();
      setResultatsAnalyse(resultats);

      navigate('/academique-resultats', {
        state: {
          idUtilisateur,
          utilisateurData: formData,
          resultatsAnalyse: resultats,
        },
      });
    } catch (err) {
      setError((err as Error).message);
    } finally {
      setIsAnalyzing(false);
    }
  };

  return (
    <div className="max-w-lg mx-auto p-4 border rounded shadow">
      <h2 className="text-xl font-bold mb-4">Formulaire d'inscription</h2>
      {error && <p className="text-red-500">{error}</p>}

      {/* Étapes du formulaire */}
      {step === 1 && (
        <input
          type="text"
          name="nom"
          placeholder="Nom"
          value={formData.nom}
          onChange={handleChange}
          className="w-full p-2 border rounded mb-2"
        />
      )}
      {step === 2 && (
        <input
          type="text"
          name="niveau_etude"
          placeholder="Niveau d'étude"
          value={formData.niveau_etude}
          onChange={handleChange}
          className="w-full p-2 border rounded mb-2"
        />
      )}
      {step === 3 && (
        <input
          type="text"
          name="filiere"
          placeholder="Filière"
          value={formData.filiere}
          onChange={handleChange}
          className="w-full p-2 border rounded mb-2"
        />
      )}
      {step === 4 && (
        <input
          type="text"
          name="domaine_interet"
          placeholder="Domaine d'intérêt"
          value={formData.domaine_interet}
          onChange={handleChange}
          className="w-full p-2 border rounded mb-2"
        />
      )}

      {/* Si l'utilisateur a été enregistré, afficher les champs de notes */}
      {idUtilisateur && matRequis.length > 0 && (
        <div className="mt-4">
          <h3 className="text-lg font-semibold">Entrez vos notes :</h3>
          {matRequis.map((subject) => (
            <div key={subject} className="flex flex-col mb-2">
              <label className="font-medium">{subject}</label>
              <input
                type="number"
                min="0"
                max="20"
                value={formData.notes[subject] || ''}
                onChange={(e) => handleGradeChange(subject, e.target.value)}
                className="w-full p-2 border rounded"
              />
            </div>
          ))}
        </div>
      )}

      {/* Boutons de navigation */}
      <div className="mt-4 flex justify-between">
        {step < 4 ? (
          <button onClick={handleNext} className="px-4 py-2 bg-green-600 text-white rounded-md">Suivant</button>
        ) : idUtilisateur ? (
          <button onClick={envoyerNotes} className="px-4 py-2 bg-blue-600 text-white rounded-md">Envoyer mes notes</button>
        ) : (
          <button onClick={handleSubmit} className="px-4 py-2 bg-green-600 text-white rounded-md">Créer mon dossier</button>
        )}
      </div>
    </div>
  );
};

export default FormulaireUtilisateur;
