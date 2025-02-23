import { ArrowLeft, Loader2 } from 'lucide-react';
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import Logo from "../assets/LogoTkT.png"


interface Question {
  id: number;
  text: string;
  options: {
    text: string;
    emoji?: string;
  }[];
}

interface UserInfo {
  firstName: string;
  lastName: string;
}

const questions: Question[] = [
  {
    id: 1,
    text: "Quel est votre niveau d'études actuel ?",
    options: [
      { text: "Baccalauréat en cours", emoji: "📚" },
      { text: "Baccalauréat obtenu", emoji: "🎓" },
      { text: "Bac+2/3", emoji: "🎯" },
      { text: "Bac+4/5 ou plus", emoji: "🌟" }
    ]
  },
  {
    id: 2,
    text: "Quels sont vos domaines d'intérêt principaux ?",
    options: [
      { text: "Technologies et Informatique", emoji: "💻" },
      { text: "Sciences et Ingénierie", emoji: "🔬" },
      { text: "Business et Management", emoji: "📊" },
      { text: "Arts et Design", emoji: "🎨" }
    ]
  },
  {
    id: 3,
    text: "Comment préférez-vous travailler ?",
    options: [
      { text: "En autonomie", emoji: "🎯" },
      { text: "En équipe", emoji: "👥" },
      { text: "Mix des deux", emoji: "🔄" },
      { text: "Leadership", emoji: "👑" }
    ]
  },
  {
    id: 4,
    text: "Quelle est votre approche face aux défis ?",
    options: [
      { text: "Analytique et méthodique", emoji: "🔍" },
      { text: "Créative et intuitive", emoji: "💡" },
      { text: "Pragmatique et orientée résultats", emoji: "🎯" },
      { text: "Collaborative et communicative", emoji: "🤝" }
    ]
  },
  {
    id: 5,
    text: "Quels sont vos objectifs professionnels ?",
    options: [
      { text: "Innovation et création", emoji: "🚀" },
      { text: "Stabilité et expertise", emoji: "📈" },
      { text: "Impact social", emoji: "🌍" },
      { text: "Entrepreneuriat", emoji: "💼" }
    ]
  },
  {
    id: 6,
    text: "Quelle est votre situation géographique préférée ?",
    options: [
      { text: "Grande ville", emoji: "🌆" },
      { text: "International", emoji: "✈️" },
      { text: "Ville moyenne", emoji: "🏢" },
      { text: "Flexible/Remote", emoji: "🏠" }
    ]
  }
];

function QuestionnairePage() {
  const navigate = useNavigate();
  const [step, setStep] = useState<'info' | 'questions' | 'loading' | 'complete'>('info');
  const [currentQuestion, setCurrentQuestion] = useState(0);
  const [selectedAnswers, setSelectedAnswers] = useState<number[]>([]);
  const [userInfo, setUserInfo] = useState<UserInfo>({ firstName: '', lastName: '' });
  const progress = ((currentQuestion + 1) / questions.length) * 100;

  const handleAnswer = (answerIndex: number) => {
    const newAnswers = [...selectedAnswers];
    newAnswers[currentQuestion] = answerIndex;
    setSelectedAnswers(newAnswers);

    if (currentQuestion === questions.length - 1) {
      setStep('loading');
      setTimeout(() => {
        setStep('complete');
      }, 4000);
    } else {
      setCurrentQuestion(prev => prev + 1);
    }
  };

  const handleUserInfoSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (userInfo.firstName && userInfo.lastName) {
      setStep('questions');
    }
  };

  const handleViewResults = () => {
    navigate('/results', { 
      state: { 
        userInfo,
        answers: selectedAnswers,
        questions
      }
    });
  };

  if (step === 'info') {
    return (
      <div className="min-h-screen bgCMC text-white">
      <div className="flex justify-center items-center">
        <img src={Logo} alt="Logo" width={300} />
      </div>

        <div className="container mx-auto px-6 py-12 max-w-lg">

          <h2 className="text-4xl font-bold mb-8 text-center">Bienvenue sur TEKTAL-<span className='text-yellow-400'>AI</span></h2>
          <form onSubmit={handleUserInfoSubmit} className="space-y-6">
            <div>
              <label htmlFor="firstName" className="block text-sm font-medium text-gray-300 mb-2">
                Prénom
              </label>
              <input
                type="text"
                id="firstName"
                value={userInfo.firstName}
                onChange={(e) => setUserInfo(prev => ({ ...prev, firstName: e.target.value }))}
                className="w-full px-4 py-2 rounded-lg bg-gray-800 border border-gray-700 text-white focus:ring-2 focus:ring-green-500 focus:border-transparent"
                required
              />
            </div>
            <div>
              <label htmlFor="lastName" className="block text-sm font-medium text-gray-300 mb-2">
                Nom
              </label>
              <input
                type="text"
                id="lastName"
                value={userInfo.lastName}
                onChange={(e) => setUserInfo(prev => ({ ...prev, lastName: e.target.value }))}
                className="w-full px-4 py-2 rounded-lg bg-gray-800 border border-gray-700 text-white focus:ring-2 focus:ring-green-500 focus:border-transparent"
                required
              />
            </div>
            <button
              type="submit"
              className="w-full bg-green-900 hover:bg-green-700 text-white text-xl px-6 py-3 rounded-lg font-semibold transition-colors"
            >
              Commencer 
     
            </button>
          </form>
        </div>
      </div>
    );
  }

  if (step === 'loading') {
    return (
      <div className="min-h-screen bgCMC text-white flex flex-col items-center justify-center">
        <Loader2 className="w-12 h-12 text-green-900 animate-spin mb-4" />
        <h2 className="text-2xl font-semibold mb-2">Analyse en cours</h2>
        <p className="text-gray-400">Nous examinons votre dossier...</p>
      </div>
    );
  }

  if (step === 'complete') {
    return (
      <div className="min-h-screen bgCMC text-white flex flex-col items-center justify-center">
        <div className="text-center">
          <div className="w-16 h-16 bg-green-700 rounded-full flex items-center justify-center mb-6 mx-auto">
            <svg className="w-8 h-8 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7" />
            </svg>
          </div>
          <h2 className="text-2xl font-semibold mb-2">Analyse terminée</h2>
          <p className="text-gray-400 mb-8">Votre profil a été analysé avec succès</p>
          <button
            onClick={handleViewResults}
            className="bg-green-900 hover:bg-green-600 text-white text-xl px-8 py-3 rounded-lg font-semibold transition-colors"
          >
            Voir mon dossier ➝
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bgCMC text-white">
      {/* Navigation */}
      <nav className="px-6 py-4 flex justify-between items-center">
        <button
          onClick={() => navigate('/')}
          className="text-gray-400 hover:text-white transition-colors flex items-center gap-2"
        >
          <ArrowLeft className="w-5 h-5" />
          Retour
        </button>
        <div className="text-sm text-gray-400">
          {currentQuestion + 1}/{questions.length}
        </div>
      </nav>

      {/* Progress bar */}
      <div className="w-full bg-gray-800 h-1">
        <div
          className="bg-yellow-500 rounded h-1 transition-all duration-300"
          style={{ width: `${progress}%` }}
        />
      </div>

      {/* Question content */}
      <div className="container mx-auto px-6 py-12 max-w-2xl">
        <h2 className="text-3xl font-bold mb-8">
          {questions[currentQuestion].text}
        </h2>

        <div className="space-y-4">
          {questions[currentQuestion].options.map((option, index) => (
            <button
              key={index}
              onClick={() => handleAnswer(index)}
              className={`w-full p-4 rounded-lg border text-left transition-all
                ${selectedAnswers[currentQuestion] === index
                  ? 'border-blue-500 bg-blue-500/10'
                  : 'border-gray-700 hover:border-gray-600 bg-gray-800/50'
                }`}
            >
              <div className="flex items-center justify-between">
                <span>{option.text}</span>
                {option.emoji && <span className="text-2xl">{option.emoji}</span>}
              </div>
            </button>
          ))}
        </div>
      </div>
    </div>
  );
}

export default QuestionnairePage;