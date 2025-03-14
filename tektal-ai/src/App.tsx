import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import LandingPage from './pages/LandingPage';
import QuestionnairePage from './pages/QuestionnairePage';
import ResultsPage from './pages/ResultsPage';
import MonGuide from './pages/MonGuide';
import AcademicOrientation from './pages/Academique/AcademicOrientation';
import DomaineAcademique from './pages/Academique/DomaineAcademique';
import FormulaireOrientation from './pages/Academique/FormulaireOrientation';

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/monguide" element={<MonGuide />} />
        {/*----- ACADEMIQUE ------*/}
        <Route path="/monguide-academique" element={<AcademicOrientation />} />
        <Route path="/monguide-domaine-academique" element={<DomaineAcademique />} />
        <Route path="/monguide-orientation-academique" element={<FormulaireOrientation />} />
        {/*----- FIN ACADEMIQUE ------*/}

        {/*----- ACADEMIQUE ------*/}
        <Route path="/monguide-professionnel" element={<AcademicOrientation />} />
        <Route path="/monguide-domaine-professionnel" element={<DomaineAcademique />} />
        <Route path="/monguide-orientation-professionnel" element={<FormulaireOrientation />} />
        {/*----- FIN ACADEMIQUE ------*/}


        <Route path="/questionnaire" element={<QuestionnairePage />} />
        <Route path="/results" element={<ResultsPage />} />
      </Routes>
    </Router>
  );
}

export default App;