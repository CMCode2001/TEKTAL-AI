import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';

const FormulaireOrientation = () => {
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
  const [isLoading, setIsLoading] = useState(false);
  const [processingMessage, setProcessingMessage] = useState('');
  const [domaines, setDomaines] = useState<string[]>([]); // Initialisé comme un tableau vide
  const [selectedDomaines, setSelectedDomaines] = useState<string[]>([]);
  const navigate = useNavigate();

  useEffect(() => {
    // Fetch domaines from the API
    const fetchDomaines = async () => {
      try {
        const response = await fetch('http://localhost:8000/domaines');
        if (!response.ok) throw new Error('Erreur lors de la récupération des domaines');

        const data = await response.json();

        // Vérifiez que la réponse contient bien un tableau `domaines`
        if (data.domaines && Array.isArray(data.domaines)) {
          setDomaines(data.domaines); // Utilisez data.domaine pour extraire le tableau
        } else {
          throw new Error("La réponse de l'API ne contient pas un tableau de domaines");
        }
      } catch (err) {
        setError((err as Error).message);
        setDomaines([]); // Assurez-vous que domaines reste un tableau vide en cas d'erreur
      }
    };

    fetchDomaines();
  }, []);

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
    if (step < 4) setStep(step + 1);
  };

  const handlePrevious = () => {
    if (step > 1) setStep(step - 1);
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
          domaine_interet: selectedDomaines.join(', '), // Utilisez selectedDomaines
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

    if (Object.keys(formData.notes).length !== matRequis.length) {
      setError("Veuillez entrer une note pour chaque matière.");
      return;
    }

    setIsAnalyzing(true);
    setError(null);
    setIsLoading(true);
    setProcessingMessage('Votre dossier est en cours de traitement...');

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

      setTimeout(() => {
        navigate('/academique-resultats', {
          state: {
            idUtilisateur,
            utilisateurData: formData,
            resultatsAnalyse: resultats,
          },
        });
      }, 4000);
    } catch (err) {
      setError((err as Error).message);
      setIsLoading(false);
    } finally {
      setIsAnalyzing(false);
    }
  };

  const handleDomaineClick = (domaine: string) => {
    setSelectedDomaines((prev) =>
      prev.includes(domaine)
        ? prev.filter((d) => d !== domaine)
        : [...prev, domaine]
    );
    setFormData((prev) => ({
      ...prev,
      domaine_interet: prev.domaine_interet ? `${prev.domaine_interet}, ${domaine}` : domaine,
    }));
  };

  const getRandomColor = () => {
    const colors = ['bg-blue-500', 'bg-green-500', 'bg-yellow-500', 'bg-red-500', 'bg-purple-500'];
    return colors[Math.floor(Math.random() * colors.length)];
  };

  return (
    <div className="flex justify-center items-center min-h-screen bgCMC">
      <div className="w-full max-w-3xl mx-auto p-6 bg-white rounded-lg shadow-lg">
        <h2 className="text-2xl font-bold text-center mb-6">S'orienter avec TEKTAL-Ai !</h2>
        {error && <p className="text-red-500 text-center mb-4">{error}</p>}

        {/* Barre de progression */}
        <div className="w-full bg-gray-200 rounded-full h-2.5 mb-6">
          <div className="bg-yellow-400 h-2.5 rounded-full" style={{ width: `${(step / 4) * 100}%` }}></div>
        </div>

        {/* Étapes du formulaire */}
        {!isLoading ? (
          <div className="space-y-4">
            {step === 1 && (
              <div> 
                <h2 className="text-xl font-semibold mb-4"> 📜 Informations Personnelles</h2>
                <label className="block text-sm font-medium text-gray-700"> ➪ Quel est votre nom complet ?</label>
                <input
                  type="text"
                  name="nom"
                  placeholder="Entrez votre nom"
                  value={formData.nom}
                  onChange={handleChange}
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                />
              </div>
            )}
            {step === 2 && (
              <div>
                <label className="block text-xl font-medium text-gray-700">➪ Quel est votre niveau d'etude ? </label>
              <p>Baccalaureat →  <b>bac</b> <br/>
              </p>
                <input
                  type="text"
                  name="niveau_etude"
                  placeholder="Entrez votre niveau d'étude"
                  value={formData.niveau_etude}
                  onChange={handleChange}
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                />
              </div>
            )}
            {step === 3 && (
              <div>
                <label className="block text-xl font-medium text-gray-700"> 🎯 Filière
                <p> ➪ Quelles filières avez-vous suivies ? </p>
                </label>
                
                <p>Scientifique → <b>s</b> <br/> Litteraire → <b>l</b> <br/>
                 Technique → <b>t</b>
                 </p>
                <input
                  type="text"
                  name="filiere"
                  placeholder="Entrez votre filière"
                  value={formData.filiere}
                  onChange={handleChange}
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                />
              </div>
            )}
            {step === 4 && (
              <div>
                <label className="block text-sm font-medium text-gray-700"> 🤔 Domaine d'intérêt</label>
                <p> ➪ Dans quels domaines d'intérêt souhaitez-vous évoluer plus tard ?</p>
                <input
                  type="text"
                  name="domaine_interet"
                  placeholder="Entrez votre domaine d'intérêt"
                  value={formData.domaine_interet}
                  onChange={handleChange}
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                />
                <div className="flex flex-wrap gap-2 mt-2">
                  {domaines.map((domaine) => (
                    <button
                      key={domaine}
                      onClick={() => handleDomaineClick(domaine)}
                      className={`px-4 py-2 rounded-full text-sm font-medium ${
                        selectedDomaines.includes(domaine)
                          ? `${getRandomColor()} text-white`
                          : 'bg-gray-200 text-gray-700'
                      }`}
                    >
                      {domaine}
                    </button>
                  ))}
                </div>
              </div>
            )}

            {/* Si l'utilisateur a été enregistré, afficher les champs de notes */}
            {idUtilisateur && matRequis.length > 0 && (
              <div className="mt-6">
                <h3 className="text-lg font-semibold mb-4">Entrez vos notes :</h3>
                {matRequis.map((subject) => (
                  <div key={subject} className="mb-4">
                    <label className="block text-sm font-medium text-gray-700">{subject}</label>
                    <input
                      type="number"
                      min="0"
                      max="20"
                      value={formData.notes[subject] || ''}
                      onChange={(e) => handleGradeChange(subject, e.target.value)}
                      className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                    />
                  </div>
                ))}
              </div>
            )}
          </div>
        ) : (
          <div className="text-center">
            {/* Spinner de chargement */}
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-500 mx-auto mb-4"></div>
            <p className="text-gray-700">{processingMessage}</p>
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
                👈🏿 Précédent
              </button>
            )}
            {step < 4 ? (
              <button
                onClick={handleNext}
                className="px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700"
              >
                Suivant 👉🏿
              </button>
            ) : idUtilisateur ? (
              <button
                onClick={envoyerNotes}
                className="px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700"
              >
                Envoyer mes notes
              </button>
            ) : (
              <button
                onClick={handleSubmit}
                className="px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700"
              >
                Envoyer 👌🏾
              </button>
            )}
          </div>
        )}
      </div>
    </div>
  );
};

export default FormulaireOrientation;