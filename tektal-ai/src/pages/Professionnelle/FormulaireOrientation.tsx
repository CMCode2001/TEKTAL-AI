import React, { useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { ChevronLeft, ChevronRight, Loader2 } from 'lucide-react';

const educationLevels = [
  'Baccalauréat',
  'Licence 1',
  'Licence 2',
  'Licence 3',
  'Master 1',
  'Master 2'
];

const streams = [
  'Sciences',
  'Littérature',
  'Économie',
  'Technologie',
  'Arts'
];

const subjectsByStream = {
  Sciences: ['Mathématiques', 'Physique', 'Chimie', 'SVT'],
  Littérature: ['Français', 'Philosophie', 'Histoire', 'Langues'],
  Économie: ['Mathématiques', 'Économie', 'Gestion', 'Droit'],
  Technologie: ['Mathématiques', 'Physique', 'Informatique', 'Électronique'],
  Arts: ['Histoire de l\'art', 'Dessin', 'Design', 'Arts plastiques']
};

function FormulaireOrientation() {
  const navigate = useNavigate();
  const [step, setStep] = useState(1);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [analysisComplete, setAnalysisComplete] = useState(false);
  const [formData, setFormData] = useState({
    name: '',
    educationLevel: '',
    stream: '',
    subjects: {} as Record<string, number>
  });

  const handleNext = () => {
    setStep(step + 1);
  };

  const handlePrevious = () => {
    setStep(step - 1);
  };

  const handleGradeChange = (subject: string, value: string) => {
    const grade = Math.min(20, Math.max(0, Number(value) || 0));
    setFormData(prev => ({
      ...prev,
      subjects: {
        ...prev.subjects,
        [subject]: grade
      }
    }));
  };

  const handleSubmit = () => {
    setIsAnalyzing(true);
    // Simuler l'analyse
    setTimeout(() => {
      setIsAnalyzing(false);
      setAnalysisComplete(true);
    }, 4000);
  };

  const renderStep = () => {
    if (isAnalyzing) {
      return (
        <div className="flex flex-col items-center justify-center space-y-4 py-12">
          <Loader2 className="w-12 h-12 text-green-600 animate-spin" />
          <p className="text-lg font-medium text-gray-800">Dossier en cours d'analyse</p>
        </div>
      );
    }

    if (analysisComplete) {
      return (
        <div className="flex flex-col items-center justify-center space-y-6 py-12">
          <div className="text-center">
            <h2 className="text-2xl font-bold text-gray-800 mb-2">Analyse Terminée</h2>
            <p className="text-gray-600">Votre profil a été analysé avec succès</p>
          </div>
          <button
            onClick={() => navigate('/academic/results')}
            className="flex items-center px-6 py-3 bg-green-600 text-white rounded-lg hover:bg-green-900 transition-colors font-bold"
          >
            Voir mon dossier
            <ChevronRight className="w-6  ml-2" />
          </button>
        </div>
      );
    }

    switch (step) {
      case 1:
        return (
          <div className="space-y-4 ">
            <h2 className="text-xl font-semibold mb-4">Informations Personnelles</h2>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Nom Complet
              </label>
              <input
                type="text"
                className="w-full p-2 border rounded-md"
                value={formData.name}
                onChange={(e) => setFormData({ ...formData, name: e.target.value })}
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Niveau d'Études
              </label>
              <div className="grid grid-cols-2 gap-2">
                {educationLevels.map((level) => (
                  <label
                    key={level}
                    className={`p-2 border rounded-md cursor-pointer ${
                      formData.educationLevel === level
                        ? 'bg-yellow-100 border-black-500'
                        : 'hover:bg-gray-50'
                    }`}
                  >
                    <input
                      type="radio"
                      name="educationLevel"
                      value={level}
                      checked={formData.educationLevel === level}
                      onChange={(e) =>
                        setFormData({ ...formData, educationLevel: e.target.value })
                      }
                      className="sr-only"
                    />
                    {level}
                  </label>
                ))}
              </div>
            </div>
          </div>
        );
      case 2:
        return (
          <div className="space-y-4">
            <h2 className="text-xl font-semibold mb-4">Filière</h2>
            <div className="grid grid-cols-2 gap-2">
              {streams.map((stream) => (
                <label
                  key={stream}
                  className={`p-2 border rounded-md cursor-pointer ${
                    formData.stream === stream
                      ? 'bg-yellow-100 border-black-500'
                      : 'hover:bg-gray-50'
                  }`}
                >
                  <input
                    type="radio"
                    name="stream"
                    value={stream}
                    checked={formData.stream === stream}
                    onChange={(e) =>
                      setFormData({ ...formData, stream: e.target.value })
                    }
                    className="sr-only"
                  />
                  {stream}
                </label>
              ))}
            </div>
          </div>
        );
      case 3:
        return (
          <div className="space-y-4">
            <h2 className="text-xl font-semibold mb-4">Notes des Matières</h2>
            <p className="text-sm text-gray-600 mb-4">
              Veuillez entrer vos notes sur 20 pour chaque matière
            </p>
            <div className="grid gap-4">
              {formData.stream && subjectsByStream[formData.stream as keyof typeof subjectsByStream].map((subject) => (
                <div key={subject} className="space-y-1">
                  <label className="block text-sm font-medium text-gray-700">
                    {subject}
                  </label>
                  <input
                    type="number"
                    min="0"
                    max="20"
                    className="w-full p-2 border rounded-md"
                    value={formData.subjects[subject] || ''}
                    onChange={(e) => handleGradeChange(subject, e.target.value)}
                  />
                </div>
              ))}
            </div>
          </div>
        );
      case 4:
        return (
          <div className="space-y-4">
            <h2 className="text-xl font-semibold mb-4">Résumé</h2>
            <div className="bg-white p-4 rounded-md border space-y-2">
              <p><strong>Nom:</strong> {formData.name}</p>
              <p><strong>Niveau d'Études:</strong> {formData.educationLevel}</p>
              <p><strong>Filière:</strong> {formData.stream}</p>
              <div className="mt-4">
                <strong>Notes:</strong>
                <div className="mt-2 grid gap-2">
                  {Object.entries(formData.subjects).map(([subject, grade]) => (
                    <p key={subject}>
                      {subject}: {grade}/20
                    </p>
                  ))}
                </div>
              </div>
            </div>
          </div>
        );
      default:
        return null;
    }
  };

  return (
    <div className="min-h-screen p-10 bgCMC">
      <div className="max-w-4xl mx-auto bg-white rounded-xl shadow-lg p-8">
        <div className="mb-8">
          <div className="flex justify-between items-center mb-4">
            <h1 className="text-2xl font-bold text-gray-800">
              Formulaire d'Orientation
            </h1>
            {!isAnalyzing && !analysisComplete && (
              <div className="text-sm text-gray-500">
                Étape {step} sur 4
              </div>
            )}
          </div>
          {!isAnalyzing && !analysisComplete && (
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div
                className="bg-green-600 h-2 rounded-full transition-all duration-300"
                style={{ width: `${(step / 4) * 100}%` }}
              ></div>
            </div>
          )}
        </div>

        {renderStep()}

        {!isAnalyzing && !analysisComplete && (
          <div className="mt-8 flex justify-between">
            {step > 1 && (
              <button
                onClick={handlePrevious}
                className="flex items-center px-4 py-2 text-gray-600 hover:text-gray-800"
              >
                <ChevronLeft className="w-4 h-4 mr-1" />
                Précédent
              </button>
            )}
            {step < 4 ? (
              <button
                onClick={handleNext}
                className="flex items-center px-4 py-2 bg-green-600 text-white rounded-md hover:bg-yellow-600 ml-auto"
              >
                Suivant
                <ChevronRight className="w-4 h-4 ml-1" />
              </button>
            ) : (
              <button
                onClick={handleSubmit}
                className="flex items-center px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-900 ml-auto"
              >
                Analyser mon profil
              </button>
            )}
          </div>
        )}
      </div>

      
    </div>
  );
}

export default FormulaireOrientation;