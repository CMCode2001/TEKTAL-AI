import { useEffect, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { ArrowLeft, ChevronRight, Loader2 } from 'lucide-react';

interface Utilisateur {
  nom: string;
  niveau_etude: string;
  filiere: string;
  domaine_interet: string;
  notes?: Record<string, number>;
}

function FormulaireOrientation() {
  const navigate = useNavigate();
  const [step, setStep] = useState(1);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [analysisComplete, setAnalysisComplete] = useState(false);
  const [idUtilisateur, setIdUtilisateur] = useState<number | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [formData, setFormData] = useState<Utilisateur>({
    nom: '',
    niveau_etude: '',
    filiere: '',
    domaine_interet: '',
    notes: {},
  });
  const [niveauxEtude, setNiveauxEtude] = useState<string[]>([]);
  const [filieres, setFilieres] = useState<string[]>([]);
  const [domaines, setDomaines] = useState<string[]>([]);
  const [selectedDomaine, setSelectedDomaine] = useState<string | null>(null);
  const [matieresRequises, setMatieresRequises] = useState<string[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [resultatsAnalyse, setResultatsAnalyse] = useState<any>(null); // Nouvel état pour stocker les résultats

  const handleNext = () => {
    if (validateStep(step)) {
      setStep(step + 1);
    }
  };

  const handlePrevious = () => setStep(step - 1);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleDomaineChange = (domaine: string) => {
    setSelectedDomaine(domaine);
    setFormData((prev) => ({ ...prev, domaine_interet: domaine }));
  };

  const handleGradeChange = (subject: string, value: string) => {
    const grade = Math.min(20, Math.max(0, Number(value) || 0));
    setFormData((prev) => ({
      ...prev,
      notes: { ...prev.notes, [subject]: grade },
    }));
  };

  const validateStep = (step: number): boolean => {
    switch (step) {
      case 1:
        if (!formData.nom || !formData.niveau_etude) {
          setError('Veuillez remplir tous les champs obligatoires.');
          return false;
        }
        break;
      case 2:
        if (!formData.filiere) {
          setError('Veuillez sélectionner une filière.');
          return false;
        }
        break;
      case 3:
        if (!selectedDomaine) {
          setError('Veuillez sélectionner un domaine d\'intérêt.');
          return false;
        }
        break;
      case 4:
        if (Object.keys(formData.notes).length !== matieresRequises.length) {
          setError('Veuillez saisir les notes pour toutes les matières requises.');
          return false;
        }
        break;
      default:
        break;
    }
    setError(null);
    return true;
  };

  useEffect(() => {
    const fetchNiveauxEtude = async () => {
      try {
        const response = await fetch('http://localhost:8000/niveaux_etude');
        if (!response.ok) throw new Error('Erreur lors de la récupération des niveaux d\'étude');
        const data = await response.json();
        setNiveauxEtude(data.niveaux_etude);
      } catch (err) {
        setError((err as Error).message);
      } finally {
        setIsLoading(false);
      }
    };

    fetchNiveauxEtude();
  }, []);

  useEffect(() => {
    const fetchFilieres = async () => {
      try {
        const response = await fetch('http://localhost:8000/filieres');
        if (!response.ok) throw new Error('Erreur lors de la récupération des filières');
        const data = await response.json();
        setFilieres(data.filieres);
      } catch (err) {
        setError((err as Error).message);
      }
    };

    fetchFilieres();
  }, []);

  useEffect(() => {
    const fetchDomaines = async () => {
      try {
        const response = await fetch('http://localhost:8000/domaines');
        if (!response.ok) throw new Error('Erreur lors de la récupération des domaines');
        const data = await response.json();
        setDomaines(data.domaines);
      } catch (err) {
        setError((err as Error).message);
      }
    };

    fetchDomaines();
  }, []);

  useEffect(() => {
    // Définir les matières requises en fonction de la filière sélectionnée
    if (formData.filiere) {
      const matieres = matieresParFiliere[formData.filiere] || [];
      setMatieresRequises(matieres);
      setFormData((prev) => ({ ...prev, notes: {} })); // Réinitialiser les notes
    }
  }, [formData.filiere]);

  const matieresParFiliere: Record<string, string[]> = {
    s: ['math', 'physique', 'informatique'],
    l: ['philosophie', 'histoire', 'economie'],
    // Ajoutez d'autres filières et leurs matières requises ici
  };

const handleSubmit = async () => {
  setIsAnalyzing(true);
  setError(null);

  try {
    // Étape 1 : Ajouter l'utilisateur
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
    console.log("Réponse de l'API après création de l'utilisateur :", data);

    // Vérifier que l'ID est correctement extrait
    if (!data.id_utilisateur) {
      throw new Error("ID utilisateur manquant dans la réponse de l'API");
    }

    setIdUtilisateur(data.id_utilisateur); // Stocker l'ID de l'utilisateur

    // Étape 2 : Envoyer les notes
    const requestBody = {
      id_utilisateur: data.id_utilisateur, // Utiliser l'ID récupéré
      notes: formData.notes,
    };
    // Étape 2 : Recuperation des matieres
    const matieresRequises  = {
      matiereRequise : data.resultat
    }
    

    console.log("Corps de la requête pour traiter les notes :", requestBody);

    const notesResponse = await fetch('http://localhost:8000/traiter_notes_utilisateur', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(requestBody),
    });

    if (!notesResponse.ok) throw new Error("Erreur lors du traitement des notes");

    const resultats = await notesResponse.json(); // Récupérer la réponse
    setResultatsAnalyse(resultats); // Stocker la réponse dans l'état

    // Simuler un délai de 4 secondes avant de naviguer
    setTimeout(() => {
      setIsAnalyzing(false);
      setAnalysisComplete(true);
      navigate('/academique-resultats', {
        state: {
          idUtilisateur: data.id_utilisateur, // Utiliser l'ID récupéré
          utilisateurData: formData,
          resultatsAnalyse: resultats,
        },
      });
    }, 4000); 
  } catch (err) {
    setError((err as Error).message);
    setIsAnalyzing(false);
  }
};

  return (
    <div className="min-h-screen p-10 bgCMC">
      <Link to="/monguide-academique" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <ArrowLeft className="w-6 h-6 mr-2" /> Précédent
      </Link>
      <br />
      <div className="max-w-4xl mx-auto bg-white rounded-xl shadow-lg p-8">
        <div className="mb-8">
          <div className="flex justify-between items-center mb-4">
            <h1 className="text-2xl font-bold text-gray-800">Formulaire d'Orientation</h1>
            {!isAnalyzing && !analysisComplete && (
              <div className="text-sm text-gray-500">Étape {step} sur 4</div>
            )}
          </div>
          {!isAnalyzing && !analysisComplete && (
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div className="bg-green-600 h-2 rounded-full transition-all duration-300" style={{ width: `${(step / 4) * 100}%` }}></div>
            </div>
          )}
        </div>

        {isAnalyzing ? (
          <div className="flex flex-col items-center justify-center space-y-4 py-12">
            <Loader2 className="w-12 h-12 text-green-600 animate-spin" />
            <p className="text-lg font-medium text-gray-800">Dossier en cours d'analyse...</p>
          </div>
        ) : analysisComplete ? (
          <div className="flex flex-col items-center justify-center space-y-6 py-12">
            <div className="text-center">
              <h2 className="text-2xl font-bold text-gray-800 mb-2">Analyse Terminée</h2>
              <p className="text-gray-600">Votre profil a été analysé avec succès</p>
            </div>
            <button onClick={() => navigate('/academique-resultats', {
              state: { idUtilisateur, utilisateurData: formData, resultatsAnalyse },
            })} className="flex items-center px-6 py-3 bg-green-600 text-white rounded-lg hover:bg-green-900 transition-colors font-bold">
              Voir mon dossier <ChevronRight className="w-6 ml-2" />
            </button>
          </div>
        ) : (
          <>
            {step === 1 && (
              <div className="space-y-4">
                <h2 className="text-xl font-semibold mb-4">Informations Personnelles</h2>
                <input type="text" name="nom" className="w-full p-2 border rounded-md" placeholder="Nom Complet"
                  value={formData.nom} onChange={handleChange} required />
                <select name="niveau_etude" className="w-full p-2 border rounded-md" value={formData.niveau_etude}
                  onChange={handleChange} required>
                  <option value="">Sélectionnez le niveau</option>
                  {niveauxEtude.map((niveau) => (
                    <option key={niveau} value={niveau}>{niveau}</option>
                  ))}
                </select>
              </div>
            )}

            {step === 2 && (
              <div className="space-y-4">
                <h2 className="text-xl font-semibold mb-4">Filières</h2>
                <select name="filiere" className="w-full p-2 border rounded-md" value={formData.filiere}
                  onChange={handleChange} required>
                  <option value="">Sélectionnez votre filière</option>
                  {filieres.map((filiere) => (
                    <option key={filiere} value={filiere}>{filiere}</option>
                  ))}
                </select>
              </div>
            )}

            {step === 3 && (
              <div className="space-y-4">
                <h2 className="text-xl font-semibold mb-4">Domaine d'intérêt</h2>
                {domaines.map((domaine) => (
                  <div key={domaine} className="flex items-center">
                    <input
                      type="radio"
                      id={domaine}
                      name="domaine_interet"
                      value={domaine}
                      checked={selectedDomaine === domaine}
                      onChange={() => handleDomaineChange(domaine)}
                      className="mr-2"
                    />
                    <label htmlFor={domaine}>{domaine}</label>
                  </div>
                ))}
              </div>
            )}

            {step === 4 && (
              <div className="space-y-4">
                <h2 className="text-xl font-semibold mb-4">Notes des Matières Requises</h2>
                {matieresRequises.map((subject) => (
                  <input
                    key={subject}
                    type="number"
                    min="0"
                    max="20"
                    className="w-full p-2 border rounded-md"
                    placeholder={subject}
                    onChange={(e) => handleGradeChange(subject, e.target.value)}
                    required
                  />
                ))}
              </div>
            )}

            {error && <div className="text-red-500 mb-4">{error}</div>}

            <div className="mt-8 flex justify-between">
              {step > 1 && <button onClick={handlePrevious} className="px-4 py-2 text-gray-600">Précédent</button>}
              {step < 4 ? (
                <button onClick={handleNext} className="px-4 py-2 bg-green-600 text-white rounded-md">Suivant</button>
              ) : (
                <button onClick={handleSubmit} className="px-4 py-2 bg-green-600 text-white rounded-md">Envoyer</button>
              )}
            </div>
          </>
        )}
      </div>
    </div>
  );
}

export default FormulaireOrientation;