use std::fmt::Debug;

use rand::seq::SliceRandom;
use rand::{distributions::uniform::SampleUniform, Rng};

#[derive(Debug, Clone, PartialEq)]
pub struct DNA {
    fitness: f32,
    genes: Vec<char>,
}

impl DNA {
    fn rand_int<T: PartialOrd + SampleUniform>(start: T, range: T) -> T {
        let mut rng = rand::thread_rng();
        rng.gen_range(start..range)
    }

    fn gen_chromosome() -> char {
        match Self::rand_int::<u8>(63, 123) {
            63 => 32 as char,
            64 => 46 as char,
            x => x as char,
        }
    }

    fn construct_genes(genec: usize) -> Vec<char> {
        (0..genec).map(|_| Self::gen_chromosome()).collect()
    }

    fn new(genec: usize) -> Self {
        DNA {
            fitness: 0.0,
            genes: Self::construct_genes(genec),
        }
    }

    fn calc_fitness(&mut self, target: &str) {
        let mut score = 0.0;
        let mut idx = 0;
        let target_count = target.len() as f32;
        self.genes.iter().for_each(|gene| {
            if gene == &target.chars().nth(idx).unwrap() {
                score += 1.0;
            };
            idx += 1;
        });

        self.fitness = score / target_count;
    }

    fn get_phrase(&self) -> String {
        self.genes.iter().collect()
    }

    fn crossover(&mut self, partner_dna: &mut Self) -> &mut Self {
        let last = self.genes.len();
        let midpoint = Self::rand_int(0, last);
        partner_dna.genes.drain(..midpoint);
        self.genes.splice(midpoint..last, partner_dna.genes.clone());
        self
    }

    fn mutate(&mut self, mutation_rate: f32) -> &mut Self {
        for gene in self.genes.iter_mut() {
            if Self::rand_int(0.0, 1.0) < mutation_rate {
                *gene = Self::gen_chromosome()
            }
        }
        self
    }
}

pub struct Population {
    pub dnas: Vec<DNA>,
    pub mating_pool: Vec<DNA>,
    pub generations: usize,
    pub is_finished: bool,
    pub perfect_fit: f32,
    pub target: String,
    pub mutation_rate: f32,
    pub best_phrase: Option<String>,
    pub best_fitness: f32,
}

impl Population {
    pub fn new(target: String, mutation_rate: f32, max_population: usize) -> Self {
        let dnas = (0..max_population)
            .into_iter()
            .map(|_| DNA::new(target.len()))
            .collect();
        Population {
            dnas: dnas,
            mating_pool: vec![],
            generations: 0,
            is_finished: false,
            perfect_fit: 1.0,
            target: target,
            mutation_rate: mutation_rate,
            best_phrase: None,
            best_fitness: 0.0,
        }
    }

    pub fn calc_fitness(&mut self) {
        for dna in self.dnas.iter_mut() {
            dna.calc_fitness(&self.target)
        }
    }

    pub fn natural_selection(&mut self) {
        for dna in self.dnas.iter() {
            let fitness = (dna.fitness * 100.0) as usize;
            for _n in 0..fitness {
                self.mating_pool.push(dna.clone())
            }
        }
    }

    pub fn generate(&mut self) {
        for dna in self.dnas.iter_mut() {
            let mut partner_a = self
                .mating_pool
                .choose(&mut rand::thread_rng())
                .unwrap()
                .clone();
            let mut partner_b = self
                .mating_pool
                .choose(&mut rand::thread_rng())
                .unwrap()
                .clone();
            let mut child = partner_a.crossover(&mut partner_b).clone();

            *dna = child.mutate(self.mutation_rate).clone();
        }

        self.generations += 1;
    }

    pub fn evalute(&mut self) {
        let best = self.dnas.iter().fold(DNA::new(0), |acc: DNA, dna| {
            if acc.fitness > dna.fitness {
                acc
            } else {
                dna.clone()
            }
        });

        self.best_phrase = Some(best.get_phrase());
        self.best_fitness = best.fitness;

        if best.fitness == self.perfect_fit {
            self.is_finished = true
        }
    }

    pub fn display(&self) {
        println!("At Generation => {:?}", self.generations);
        println!(
            "Best Phrase => {:?}",
            self.best_phrase.clone().unwrap_or("".to_owned())
        );
        println!("Best Fitness => {:?}", self.best_fitness);
    }
}

impl Debug for Population {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Population")
            .field("generations", &self.generations)
            .field("is_finished", &self.is_finished)
            .field("target", &self.target)
            .field("mutation_rate", &self.mutation_rate)
            .field("best_phrase", &self.best_phrase)
            .field("best_fitness", &self.best_fitness)
            .finish()
    }
    // Ok("!");
}

fn main() {
    let mut popl = Population::new("O Grand love".to_owned(), 0.01, 200);

    loop {
        popl.display();
        popl.calc_fitness();
        popl.natural_selection();
        popl.generate();
        popl.calc_fitness();
        popl.evalute();

        if popl.is_finished {
            popl.display();
            break;
        }
    }
}
