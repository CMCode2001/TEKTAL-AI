% Correspondance entre filière et domaines autorisés
correspondance_filiere_domaine(bac, s, informatique).
correspondance_filiere_domaine(bac, s, mecanique).
correspondance_filiere_domaine(bac, s, medecine).
correspondance_filiere_domaine(bac, s, electricite).
correspondance_filiere_domaine(bac, s, ingenierie).

correspondance_filiere_domaine(bac, l, droit).
correspondance_filiere_domaine(bac, l, gestion).

correspondance_filiere_domaine(bac, t, mecanique).
correspondance_filiere_domaine(bac, t, electricite).

correspondance_filiere_domaine(licence, informatique, informatique).
correspondance_filiere_domaine(licence, droit, droit).
correspondance_filiere_domaine(licence, gestion, gestion).
correspondance_filiere_domaine(licence, mecanique, mecanique).
correspondance_filiere_domaine(licence, electricite, electricite).

% Exemples de matières requises
matiere_requise(informatique, bac, s, [math, physique, informatique]).
matiere_requise(mecanique, bac, s, [math, physique, technologie]).
matiere_requise(medecine, bac, s, [svt, chimie, physique]).
matiere_requise(droit, bac, l, [philosophie, histoire, economie]).
matiere_requise(gestion, bac, l, [math, economie, gestion]).
matiere_requise(electricite, bac, s, [physique, technologie, math]).

% Établissements
etablissement(informatique, ["UCAD", "ESP", "Sup'Info"]).
etablissement(mecanique, ["CFPT", "EPT", "IUT"]).
etablissement(droit, ["Faculté de Droit UCAD", "ISM", "Sup'Déco"]).
etablissement(gestion, ["SupGestion", "BEM Dakar", "ISM"]).
etablissement(medecine, ["Faculté de Médecine UCAD", "UFR Santé"]).
etablissement(electricite, ["ESP", "IUT", "ESEBAT"]).

% Parcours de formation
parcours(informatique, bac, ["Licence en Informatique", "BTS en Développement", "Certification en Programmation"]).
parcours(mecanique, bac, ["BTS en Mécanique", "Licence en Génie Mécanique", "Formation en Maintenance Automobile"]).
parcours(medecine, bac, ["Première année Médecine", "Pharmacie", "Dentisterie"]).
parcours(droit, bac, ["Licence en Droit", "Master en Droit des Affaires", "Concours Magistrature"]).
parcours(gestion, bac, ["Licence en Gestion", "Master en Management", "Formation en Entrepreneuriat"]).
parcours(electricite, bac, ["BTS en Electricité", "Licence en Génie Electrique", "Certification en Systèmes Électriques"]).

% Correspondance entre compétences techniques et domaines
competence_technique_domaine(programmation, [informatique, intelligence_artificielle, developpement_software]).
competence_technique_domaine(reseau, [cybersecurite, telecoms, informatique]).
competence_technique_domaine(maintenance, [mecanique, electronique, electricite]).
competence_technique_domaine(gestion_projet, [management, genie_civil, entreprenariat]).
competence_technique_domaine(analyse_donnees, [data_science, intelligence_artificielle, finance]).
competence_technique_domaine(redaction, [journalisme, communication, droit]).
competence_technique_domaine(dessin_industriel, [architecture, design, genie_mecanique]).
competence_technique_domaine(chimie, [pharmacie, medecine, genie_chimique]).
competence_technique_domaine(comptabilite, [finance, gestion, audit]).

% Correspondance entre critères de personnalité et domaines
personnalite_domaine(organise, [gestion, droit, finance]).
personnalite_domaine(curieux, [recherche, journalisme, informatique]).
personnalite_domaine(rigoureux, [medecine, ingenierie, comptabilite]).
personnalite_domaine(empathetique, [psychologie, ressources_humaines, medecine]).
personnalite_domaine(ambitieux, [entrepreneuriat, politique, commerce]).
personnalite_domaine(artistique, [design, musique, cinema]).
