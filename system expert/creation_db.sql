CREATE DATABASE expert_system;
USE expert_system;

-- Table des profils
CREATE TABLE profils (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nom VARCHAR(100) UNIQUE NOT NULL,
    niveau_etude INT NOT NULL
);

-- Table des compétences
CREATE TABLE competences (
    id INT AUTO_INCREMENT PRIMARY KEY,
    profil_id INT NOT NULL,
    competence VARCHAR(100) NOT NULL,
    niveau INT NOT NULL,
    FOREIGN KEY (profil_id) REFERENCES profils(id) ON DELETE CASCADE
);

-- Table des matières préférées
CREATE TABLE matieres (
    id INT AUTO_INCREMENT PRIMARY KEY,
    profil_id INT NOT NULL,
    matiere VARCHAR(100) NOT NULL,
    niveau INT NOT NULL,
    FOREIGN KEY (profil_id) REFERENCES profils(id) ON DELETE CASCADE
);

-- Table des intérêts
CREATE TABLE interets (
    id INT AUTO_INCREMENT PRIMARY KEY,
    profil_id INT NOT NULL,
    domaine VARCHAR(100) NOT NULL,
    niveau INT NOT NULL,
    FOREIGN KEY (profil_id) REFERENCES profils(id) ON DELETE CASCADE
);

-- Table des traits de personnalité
CREATE TABLE traits_personnalite (
    id INT AUTO_INCREMENT PRIMARY KEY,
    profil_id INT NOT NULL,
    trait VARCHAR(100) NOT NULL,
    niveau INT NOT NULL,
    FOREIGN KEY (profil_id) REFERENCES profils(id) ON DELETE CASCADE
);
