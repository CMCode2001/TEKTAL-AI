% Correspondance entre filiere et domaines autorises
correspondance_filiere_domaine(bac, s, informatique).
correspondance_filiere_domaine(bac, s, mecanique).
correspondance_filiere_domaine(bac, s, medecine).
correspondance_filiere_domaine(bac, s, electricite).
correspondance_filiere_domaine(bac, s, ingenierie).
correspondance_filiere_domaine(bac, s, biologie).
correspondance_filiere_domaine(bac, s, chimie).

correspondance_filiere_domaine(bac, l, droit).
correspondance_filiere_domaine(bac, l, gestion).
correspondance_filiere_domaine(bac, l, psychologie).
correspondance_filiere_domaine(bac, l, sociologie).

correspondance_filiere_domaine(bac, t, mecanique).
correspondance_filiere_domaine(bac, t, electricite).
correspondance_filiere_domaine(bac, t, electronique).

correspondance_filiere_domaine(licence, informatique, informatique).
correspondance_filiere_domaine(licence, droit, droit).
correspondance_filiere_domaine(licence, gestion, gestion).
correspondance_filiere_domaine(licence, mecanique, mecanique).
correspondance_filiere_domaine(licence, electricite, electricite).
correspondance_filiere_domaine(licence, psychologie, psychologie).
correspondance_filiere_domaine(licence, sociologie, sociologie).
correspondance_filiere_domaine(licence, biologie, biologie).
correspondance_filiere_domaine(licence, chimie, chimie).

% Correspondance entre Master et domaines professionnels
correspondance_filiere_domaine(master, informatique, intelligence_artificielle).
correspondance_filiere_domaine(master, informatique, developpement_software).
correspondance_filiere_domaine(master, informatique, cybersecurite).
correspondance_filiere_domaine(master, informatique, analyse_donnees).
correspondance_filiere_domaine(master, mecanique, ingenierie_mecanique).
correspondance_filiere_domaine(master, mecanique, robotique).
correspondance_filiere_domaine(master, electricite, energie_renouvelable).
correspondance_filiere_domaine(master, chimie, genie_chimique).
correspondance_filiere_domaine(master, biologie, biotechnologie).
correspondance_filiere_domaine(master, gestion, management).
correspondance_filiere_domaine(master, droit, droit_international).
correspondance_filiere_domaine(master, psychologie, psychologie_clinique).
correspondance_filiere_domaine(master, sociologie, sociologie_urbaine).


% Exemples de matieres requises
matiere_requise(informatique, bac, s, [math, physique, informatique]).
matiere_requise(mecanique, bac, s, [math, physique, technologie]).
matiere_requise(mecanique, bac, t, [math, physique, technologie]).
matiere_requise(medecine, bac, s, [svt, chimie, physique]).
matiere_requise(droit, bac, l, [philosophie, histoire, economie]).
matiere_requise(gestion, bac, l, [math, economie, gestion]).
matiere_requise(electricite, bac, s, [physique, technologie, math]).
matiere_requise(psychologie, bac, l, [philosophie, svt, histoire]).
matiere_requise(sociologie, bac, l, [philosophie, histoire, economie]).
matiere_requise(biologie, bac, s, [svt, chimie, physique]).
matiere_requise(chimie, bac, s, [chimie, physique, math]).

% etablissements
etablissement(informatique, ["UCAD", "ESP", "Sup'Info", "Polytech", "ENSI"]).
etablissement(mecanique, ["CFPT", "EPT", "IUT", "ENSAM", "ISAE"]).
etablissement(droit, ["Faculte de Droit UCAD", "ISM", "Sup'Deco", "Universite Paris 1", "Universite de Geneve"]).
etablissement(gestion, ["SupGestion", "BEM Dakar", "ISM", "HEC Paris", "ESSEC"]).
etablissement(medecine, ["Faculte de Medecine UCAD", "UFR Sante", "Universite de Montpellier", "Universite de Geneve"]).
etablissement(electricite, ["ESP", "IUT", "ESEBAT", "ENSEA", "INP Toulouse"]).
etablissement(psychologie, ["Universite de Paris", "Universite de Geneve", "Universite de Montreal", "Universite de Lausanne"]).
etablissement(sociologie, ["Universite de Paris", "Universite de Geneve", "Universite de Montreal", "Universite de Lausanne"]).
etablissement(biologie, ["Universite de Montpellier", "Universite de Geneve", "Universite de Lausanne", "Universite de Paris"]).
etablissement(chimie, ["Universite de Montpellier", "Universite de Geneve", "Universite de Lausanne", "Universite de Paris"]).

% Parcours de formation
parcours(informatique, bac, ["Licence en Informatique", "BTS en Developpement", "Certification en Programmation", "Master en Intelligence Artificielle", "Doctorat en Informatique"]).
parcours(mecanique, bac, ["BTS en Mecanique", "Licence en Genie Mecanique", "Formation en Maintenance Automobile", "Master en Genie Mecanique", "Doctorat en Mecanique"]).
parcours(medecine, bac, ["Premiere annee Medecine", "Pharmacie", "Dentisterie", "Master en Sante Publique", "Doctorat en Medecine"]).
parcours(droit, bac, ["Licence en Droit", "Master en Droit des Affaires", "Concours Magistrature", "Doctorat en Droit", "Master en Droit International"]).
parcours(gestion, bac, ["Licence en Gestion", "Master en Management", "Formation en Entrepreneuriat", "MBA", "Doctorat en Gestion"]).
parcours(electricite, bac, ["BTS en Electricite", "Licence en Genie Electrique", "Certification en Systemes electriques", "Master en energie", "Doctorat en electrotechnique"]).
parcours(psychologie, bac, ["Licence en Psychologie", "Master en Psychologie Clinique", "Formation en Neuropsychologie", "Doctorat en Psychologie", "Master en Psychologie Sociale"]).
parcours(sociologie, bac, ["Licence en Sociologie", "Master en Sociologie Urbaine", "Formation en Demographie", "Doctorat en Sociologie", "Master en Sociologie Politique"]).
parcours(biologie, bac, ["Licence en Biologie", "Master en Biologie Moleculaire", "Formation en Biotechnologie", "Doctorat en Biologie", "Master en ecologie"]).
parcours(chimie, bac, ["Licence en Chimie", "Master en Chimie Analytique", "Formation en Chimie Organique", "Doctorat en Chimie", "Master en Chimie des Materiaux"]).

% Correspondance entre competences techniques et domaines
competence_technique_domaine(programmation, [informatique, intelligence_artificielle, developpement_software]).
competence_technique_domaine(reseau, [cybersecurite, telecoms, informatique]).
competence_technique_domaine(maintenance, [mecanique, electronique, electricite]).
competence_technique_domaine(gestion_projet, [management, genie_civil, entreprenariat]).
competence_technique_domaine(analyse_donnees, [data_science, intelligence_artificielle, finance]).
competence_technique_domaine(redaction, [journalisme, communication, droit]).
competence_technique_domaine(dessin_industriel, [architecture, design, genie_mecanique]).
competence_technique_domaine(chimie, [pharmacie, medecine, genie_chimique]).
competence_technique_domaine(comptabilite, [finance, gestion, audit]).
competence_technique_domaine(statistiques, [data_science, economie, psychologie]).
competence_technique_domaine(design_graphique, [design, communication, marketing]).
competence_technique_domaine(securite_informatique, [cybersecurite, informatique, telecoms]).
competence_technique_domaine(genie_logiciel, [informatique, developpement_software, intelligence_artificielle]).
competence_technique_domaine(marketing_digital, [marketing, communication, gestion]).
competence_technique_domaine(enseignement, [education, psychologie, sociologie]).

% Correspondance entre criteres de personnalite et domaines
personnalite_domaine(organise, [gestion, droit, finance]).
personnalite_domaine(curieux, [recherche, journalisme, informatique]).
personnalite_domaine(rigoureux, [medecine, ingenierie, comptabilite]).
personnalite_domaine(empathetique, [psychologie, ressources_humaines, medecine]).
personnalite_domaine(ambitieux, [entrepreneuriat, politique, commerce]).
personnalite_domaine(artistique, [design, musique, cinema]).
personnalite_domaine(analytique, [data_science, economie, psychologie]).
personnalite_domaine(communicatif, [journalisme, communication, marketing]).
personnalite_domaine(creatif, [design, art, publicite]).
personnalite_domaine(resolu, [ingenierie, gestion, entrepreneuriat]).
personnalite_domaine(patient, [medecine, psychologie, education]).
personnalite_domaine(leader, [management, politique, entrepreneuriat]).

% Correspondance entre domaines et metiers
correspondance_metier_domaine(informatique, [developpeur_software, ingenieur_reseau, data_scientist, expert_cybersecurite, architecte_logiciel, devops_engineer, scrum_master, product_owner]).
correspondance_metier_domaine(mecanique, [ingenieur_mecanique, technicien_maintenance, concepteur_CAO, expert_robotique, ingenieur_thermique, ingenieur_fluides]).
correspondance_metier_domaine(medecine, [medecin_generaliste, chirurgien, pharmacien, dentiste, radiologue, anesthesiste, pediatre, gynecologue]).
correspondance_metier_domaine(droit, [avocat, notaire, juge, juriste, consultant_juridique, procureur, greffier, mediateur]).
correspondance_metier_domaine(gestion, [manager, comptable, auditeur_financier, consultant_strategie, directeur_ressources_humaines, chef_de_projet, analyste_financier]).
correspondance_metier_domaine(electricite, [ingenieur_electrique, technicien_reseau, expert_energetique, electricien_industriel, ingenieur_systemes_energetiques]).
correspondance_metier_domaine(psychologie, [psychologue_clinicien, psychologue_du_travail, neuropsychologue, psychotherapeute, conseiller_orientation]).
correspondance_metier_domaine(sociologie, [chercheur_en_sociologie, analyste_politique, conseiller_social, urbaniste, consultant_en_developpement_communautaire]).
correspondance_metier_domaine(biologie, [chercheur_en_biologie, microbiologiste, geneticien, biologiste_marin, bioinformaticien, ecologiste]).
correspondance_metier_domaine(chimie, [chimiste, formulateur, ingenieur_chimiste, expert_en_environnement, toxicologue, ingenieur_materiaux]).

% Metiers accessibles en fonction du niveau d'etude
metier_par_niveau(bac_plus_2, informatique, [technicien_support, developpeur_web, administrateur_reseau, technicien_helpdesk]).
metier_par_niveau(licence, informatique, [developpeur_software, analyste_donnees, ingenieur_systemes, testeur_logiciel]).
metier_par_niveau(master, informatique, [data_scientist, expert_cybersecurite, architecte_logiciel, chef_de_projet_informatique]).

metier_par_niveau(bac_plus_2, mecanique, [technicien_mecanique, dessinateur_industriel, agent_de_maintenance, operateur_machine]).
metier_par_niveau(licence, mecanique, [ingenieur_mecanique, concepteur_CAO, technicien_superieur_mecanique]).
metier_par_niveau(master, mecanique, [expert_robotique, chef_de_projet_mecanique, ingenieur_recherche_et_developpement]).

metier_par_niveau(bac_plus_2, medecine, [assistant_medical, technicien_laboratoire, aide_soignant, ambulancier]).
metier_par_niveau(licence, medecine, [infirmier_specialise, technicien_analyse_biologique, assistant_pharmacien]).
metier_par_niveau(master, medecine, [medecin_generaliste, pharmacien, chirurgien, radiologue, anesthesiste]).

metier_par_niveau(bac_plus_2, droit, [assistant_juridique, greffier, agent_administratif]).
metier_par_niveau(licence, droit, [juriste, conseiller_fiscal, assistant_notaire]).
metier_par_niveau(master, droit, [avocat, notaire, juge, procureur]).

metier_par_niveau(bac_plus_2, gestion, [assistant_administratif, assistant_comptable, agent_de_vente]).
metier_par_niveau(licence, gestion, [comptable, assistant_manager, analyste_administratif]).
metier_par_niveau(master, gestion, [manager, auditeur_financier, consultant_strategie, directeur_ressources_humaines]).

metier_par_niveau(bac_plus_2, electricite, [electricien, technicien_maintenance_electrique, monteur_cables]).
metier_par_niveau(licence, electricite, [technicien_superieur_electricite, dessinateur_projet_electrique]).
metier_par_niveau(master, electricite, [ingenieur_electrique, expert_energetique, chef_de_projet_electrique]).

metier_par_niveau(bac_plus_2, psychologie, [assistant_psychologue, conseiller_orientation, educateur_specialise]).
metier_par_niveau(licence, psychologie, [psychologue_du_travail, conseiller_psychosocial]).
metier_par_niveau(master, psychologie, [psychologue_clinicien, neuropsychologue, psychotherapeute]).

metier_par_niveau(bac_plus_2, sociologie, [assistant_social, animateur_socioculturel]).
metier_par_niveau(licence, sociologie, [conseiller_social, assistant_recherche_sociologie]).
metier_par_niveau(master, sociologie, [chercheur_en_sociologie, analyste_politique, urbaniste]).

metier_par_niveau(bac_plus_2, biologie, [technicien_laboratoire_biologie, assistant_recherche_biologie]).
metier_par_niveau(licence, biologie, [technicien_superieur_biologie, assistant_biologiste]).
metier_par_niveau(master, biologie, [chercheur_en_biologie, microbiologiste, geneticien]).

metier_par_niveau(bac_plus_2, chimie, [technicien_laboratoire_chimie, operateur_procede_chimique]).
metier_par_niveau(licence, chimie, [technicien_superieur_chimie, assistant_chimiste]).
metier_par_niveau(master, chimie, [chimiste, ingenieur_chimiste, expert_en_environnement]).


% Verifier que l'utilisateur a au moins Bac+2 avant de recommander un metier
niveau_etude_valide(bac_plus_2).   % (DUT, BTS, DEUG, etc.)
niveau_etude_valide(licence).      % Licence (Bac+3)
niveau_etude_valide(master).       % Master (Bac+5)
