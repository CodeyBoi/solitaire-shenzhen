use core::panic;
use std::{
    collections::{HashMap, HashSet},
    env,
    fmt::Display,
    fs::File,
    io::{stdin, BufRead, Read, Write},
};

use clap::{arg, command, Parser};
use rand::{
    rngs::{StdRng, ThreadRng},
    seq::SliceRandom,
    thread_rng, Rng, SeedableRng,
};

#[derive(Clone)]
struct Solitaire {
    columns: Vec<Vec<Card>>,
    card_cells: Vec<CardCell>,
    card_stacks: Vec<CardStack>,
}

#[derive(Clone, Copy)]
struct CardStack {
    suit: Suit,
    value: u8,
}

impl Display for CardStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.value == 0 {
            write!(f, "[]")
        } else {
            write!(
                f,
                "{}{}",
                self.suit.to_char(),
                char::from_digit(self.value as u32, 10).expect("invalid card value")
            )
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Suit {
    Red,
    Black,
    Green,
}

impl Suit {
    pub fn to_char(self) -> char {
        match self {
            Suit::Red => 'R',
            Suit::Black => 'B',
            Suit::Green => 'G',
        }
    }
}

impl Display for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_char())
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Card {
    Dragon(Suit),
    Flower,
    Numbered { suit: Suit, value: u8 },
}

#[derive(Clone, Copy)]
enum CardCell {
    Used(Card),
    Dragons,
}

impl Display for CardCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Used(card) => write!(f, "{}", card),
            Self::Dragons => write!(f, "><"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    Move { from: u8, to: u8, amount: u8 },
    StoreCard(u8),
    PlaceCard { cell: u8, column: u8 },
    StackCard(u8),
    StackCardFromCell(u8),
    CollectDragons(Suit),
}

impl Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Move { from, to, amount } => write!(
                f,
                "m{}{}{}",
                if *amount > 1 {
                    amount.to_string()
                } else {
                    "".to_string()
                },
                from + 1,
                to + 1
            ),
            Action::StoreCard(column) => write!(f, "s{}", column + 1),
            Action::PlaceCard { cell, column } => write!(f, "p{}{}", cell + 1, column + 1),
            Action::StackCard(column) => write!(f, "r{}", column + 1),
            Action::StackCardFromCell(cell) => write!(f, "R{}", cell + 1),
            Action::CollectDragons(suit) => write!(
                f,
                "d{}",
                match suit {
                    Suit::Red => "r",
                    Suit::Black => "b",
                    Suit::Green => "g",
                }
            ),
        }
    }
}

impl Card {
    fn can_place_on(&self, other: Option<&Self>) -> bool {
        use Card as C;
        match (self, other) {
            (
                C::Numbered { suit, value },
                Some(C::Numbered {
                    suit: other_suit,
                    value: other_value,
                }),
            ) => value + 1 == *other_value && suit != other_suit,
            (_, None) => true,
            _ => false,
        }
    }

    fn can_stack_on(&self, other: CardStack) -> bool {
        use Card as C;
        match (self, other) {
            (
                C::Numbered { suit, value },
                CardStack {
                    suit: other_suit,
                    value: other_value,
                },
            ) => *value == other_value + 1 && *suit == other_suit,
            _ => false,
        }
    }

    fn from_string(s: &str) -> Option<Self> {
        let mut chs = s.chars();
        let suit = match chs.next()?.to_ascii_lowercase() {
            'r' => Suit::Red,
            'b' => Suit::Black,
            'g' => Suit::Green,
            'f' => {
                if chs.next()?.to_ascii_lowercase() == 'l' {
                    return Some(Card::Flower);
                } else {
                    return None;
                }
            }
            _ => return None,
        };
        match chs.next()?.to_ascii_lowercase() {
            value @ '1'..='9' => Some(Card::Numbered {
                suit,
                value: value as u8 - b'0',
            }),
            'd' => Some(Card::Dragon(suit)),
            _ => None,
        }
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (suit, value) = match self {
            Card::Flower => ('F', 'L'),
            Card::Dragon(suit) => (suit.to_char(), 'D'),
            Card::Numbered { suit, value } => (
                suit.to_char(),
                char::from_digit(*value as u32, 10).expect("invalid card value"),
            ),
        };
        write!(f, "{}{}", suit, value)
    }
}

enum SolveResult {
    Solved(Vec<Action>),
    Unsolvable,
}

impl Solitaire {
    const COLUMNS: usize = 8;
    const STANDARD_DECK: [Card; 40] = [
        Card::Flower,
        Card::Dragon(Suit::Red),
        Card::Dragon(Suit::Red),
        Card::Dragon(Suit::Red),
        Card::Dragon(Suit::Red),
        Card::Dragon(Suit::Black),
        Card::Dragon(Suit::Black),
        Card::Dragon(Suit::Black),
        Card::Dragon(Suit::Black),
        Card::Dragon(Suit::Green),
        Card::Dragon(Suit::Green),
        Card::Dragon(Suit::Green),
        Card::Dragon(Suit::Green),
        Card::Numbered {
            suit: Suit::Red,
            value: 1,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 2,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 3,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 4,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 5,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 6,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 7,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 8,
        },
        Card::Numbered {
            suit: Suit::Red,
            value: 9,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 1,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 2,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 3,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 4,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 5,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 6,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 7,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 8,
        },
        Card::Numbered {
            suit: Suit::Black,
            value: 9,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 1,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 2,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 3,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 4,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 5,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 6,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 7,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 8,
        },
        Card::Numbered {
            suit: Suit::Green,
            value: 9,
        },
    ];

    fn get_top_cards(&self) -> Vec<Option<&Card>> {
        self.columns.iter().map(|column| column.last()).collect()
    }

    fn get_movable_cards(&self, column: u8) -> u8 {
        let column = &self.columns[column as usize];
        if column.is_empty() {
            return 0;
        }
        column
            .iter()
            .rev()
            .collect::<Vec<_>>()
            .windows(2)
            .take_while(|cards| cards[0].can_place_on(Some(cards[1])))
            .count() as u8
            + 1
    }

    fn get_possible_actions(&self) -> Vec<Action> {
        let mut actions = Vec::new();
        let has_free_card_cell = self.card_cells.len() < 3;
        let top_cards = self.get_top_cards();

        for (col_idx, column) in self
            .columns
            .iter()
            .enumerate()
            .filter(|(_, col)| !col.is_empty())
        {
            let col_idx = col_idx as u8;
            for amount in 1..=self.get_movable_cards(col_idx) {
                let card = column[column.len() - amount as usize];
                for (other_column, _) in top_cards
                    .iter()
                    .enumerate()
                    .filter(|&(_, &other_card)| card.can_place_on(other_card))
                {
                    actions.push(Action::Move {
                        from: col_idx,
                        to: other_column as u8,
                        amount,
                    });
                }
            }

            if has_free_card_cell {
                actions.push(Action::StoreCard(col_idx));
            }

            if let Some(card @ &Card::Numbered { suit: _, value: _ }) = column.last() {
                for stack in &self.card_stacks {
                    if card.can_stack_on(*stack) {
                        actions.push(Action::StackCard(col_idx));
                    }
                }
            }
        }

        for (cell_idx, cell) in self.card_cells.iter().enumerate() {
            if let CardCell::Used(card) = cell {
                for (column, _) in top_cards
                    .iter()
                    .enumerate()
                    .filter(|&(_, &other_card)| card.can_place_on(other_card))
                {
                    actions.push(Action::PlaceCard {
                        cell: cell_idx as u8,
                        column: column as u8,
                    });
                }

                for stack in &self.card_stacks {
                    if card.can_stack_on(*stack) {
                        actions.push(Action::StackCardFromCell(cell_idx as u8));
                    }
                }
            }
        }

        for suit in [Suit::Red, Suit::Black, Suit::Green] {
            if self.can_collect_dragons(suit) {
                actions.push(Action::CollectDragons(suit));
            }
        }

        actions
    }

    fn do_action(&mut self, action: Action) {
        use Action as A;
        match action {
            A::Move { from, to, amount } => self.move_cards(from, to, amount),
            A::CollectDragons(suit) => self.collect_dragons(suit),
            A::StoreCard(column) => self.store_card(column),
            A::PlaceCard { cell, column } => self.place_card(cell, column),
            A::StackCard(column) => self.stack_card(column),
            A::StackCardFromCell(cell) => self.stack_card_from_cell(cell),
        }

        self.do_all_automatic_actions();
    }

    fn move_cards(&mut self, from: u8, to: u8, amount: u8) {
        let column = &mut self.columns[from as usize];
        let mut cards = column.split_off(column.len() - amount as usize);
        self.columns[to as usize].append(&mut cards);
    }

    fn can_collect_dragons(&self, suit: Suit) -> bool {
        let dragon_columns: Vec<_> = self
            .get_top_cards()
            .iter()
            .enumerate()
            .filter_map(|(i, card)| match card {
                Some(&Card::Dragon(dragon_suit)) if suit == dragon_suit => Some(i),
                _ => None,
            })
            .collect();

        let dragon_cells: Vec<_> = self
            .card_cells
            .iter()
            .enumerate()
            .filter_map(|(i, card)| match card {
                &CardCell::Used(Card::Dragon(dragon_suit)) if suit == dragon_suit => Some(i),
                _ => None,
            })
            .collect();

        (self.card_cells.len() < 3 || !dragon_cells.is_empty())
            && dragon_columns.len() + dragon_cells.len() == 4
    }

    fn collect_dragons(&mut self, suit: Suit) {
        if !self.can_collect_dragons(suit) {
            panic!("tried to collect dragons when unable to");
        }

        let dragon_columns: Vec<_> = self
            .get_top_cards()
            .iter()
            .enumerate()
            .filter_map(|(i, card)| match card {
                Some(&Card::Dragon(dragon_suit)) if suit == dragon_suit => Some(i),
                _ => None,
            })
            .collect();
        for column in dragon_columns {
            self.columns[column].pop();
        }

        self.card_cells = self
            .card_cells
            .clone()
            .into_iter()
            .filter(|cell| !matches!(cell, CardCell::Used(Card::Dragon(dragon_suit)) if suit == *dragon_suit))
            .collect();

        self.card_cells.push(CardCell::Dragons);
    }

    fn store_card(&mut self, column: u8) {
        let card = if let Some(card) = self.columns[column as usize].pop() {
            card
        } else {
            panic!("tried to store card from column {}, which is empty", column);
        };
        if self.card_cells.len() >= 3 {
            panic!(
                "tried to store card from {} with no available card cells",
                column
            );
        }
        self.card_cells.push(CardCell::Used(card));
    }

    fn place_card(&mut self, cell: u8, column: u8) {
        let column = &mut self.columns[column as usize];
        match self.card_cells.swap_remove(cell as usize) {
            CardCell::Used(card) => {
                if card.can_place_on(column.last()) {
                    column.push(card);
                } else {
                    panic!("tried to place {} on {}", card, column.last().unwrap());
                }
            }
            CardCell::Dragons => {
                panic!("tried to place card from card cell with stacked dragons")
            }
        }
    }

    fn stack_card(&mut self, column: u8) {
        let backup = self.clone();
        let (suit, value) = match self.columns[column as usize].pop() {
            Some(Card::Numbered { suit, value }) => (suit, value),
            Some(Card::Dragon(_)) => panic!("tried to stack dragon from {}", column),
            None => panic!("tried to stack card from column {}, which is empty", column),
            _ => panic!("i have no clue what went wrong"),
        };
        let stack = self
            .card_stacks
            .iter_mut()
            .find(|stack| stack.suit == suit)
            .expect("card stack missing");
        if stack.value + 1 != value {
            panic!(
                "{}\ntried to stack {} on {}",
                backup,
                Card::Numbered { suit, value },
                stack
            );
        }
        stack.value = value;
    }

    fn has_won(&self) -> bool {
        self.columns.iter().all(Vec::is_empty)
    }

    fn stack_card_from_cell(&mut self, cell: u8) {
        let (suit, value) = match self.card_cells.swap_remove(cell as usize) {
            CardCell::Used(Card::Numbered { suit, value }) => (suit, value),
            CardCell::Used(Card::Dragon(_)) => panic!("tried to stack dragon"),
            CardCell::Dragons => panic!("tried to stack card from stacked dragons"),
            _ => panic!("i have no clue what went wrong"),
        };
        let stack = self
            .card_stacks
            .iter_mut()
            .find(|stack| stack.suit == suit)
            .expect("card stack missing");
        if stack.value + 1 != value {
            panic!(
                "tried to stack {} from card cell {} on {}",
                Card::Numbered { suit, value },
                cell,
                stack
            );
        }
        stack.value = value;
    }

    fn get_automatic_stack_limit(&self) -> u8 {
        self.card_stacks
            .iter()
            .map(|stack| stack.value)
            .min()
            .expect("card stack vector cannot be empty")
            .max(1)
            + 1
    }

    fn do_automatic_action(&mut self) -> bool {
        if let Some(flower_idx) = self
            .get_top_cards()
            .iter()
            .position(|card| matches!(card, Some(Card::Flower)))
        {
            self.columns[flower_idx].pop();
            return true;
        }

        let automatic_stack_limit = self.get_automatic_stack_limit();
        let mut action = None;
        for (col_idx, card) in self.get_top_cards().iter().enumerate() {
            if let Some(&Card::Numbered { suit, value }) = card {
                let stack = self
                    .card_stacks
                    .iter()
                    .find(|stack| stack.suit == suit)
                    .expect("all stacks must exist");
                if value <= automatic_stack_limit && stack.value + 1 == value {
                    action = Some(Action::StackCard(col_idx as u8));
                    break;
                }
            }
        }

        for (cell_idx, card) in self.card_cells.iter().enumerate() {
            if let CardCell::Used(Card::Numbered { suit, value }) = card {
                let stack = self
                    .card_stacks
                    .iter()
                    .find(|stack| stack.suit == *suit)
                    .expect("all stacks must exist");
                if *value <= automatic_stack_limit && stack.value + 1 == *value {
                    action = Some(Action::StackCardFromCell(cell_idx as u8));
                    break;
                }
            }
        }

        if let Some(action) = action {
            self.do_action(action);
            true
        } else {
            false
        }
    }

    fn do_all_automatic_actions(&mut self) {
        loop {
            if !self.do_automatic_action() {
                break;
            }
        }
    }

    fn parse_input(input: &str) -> Option<Action> {
        let mut input = input.chars();
        match input.next()? {
            'm' => {
                let arg0 = input.next()? as u8 - b'0';
                let arg1 = input.next()? as u8 - b'0';
                if let Some(arg2) = input.next() {
                    Some(Action::Move {
                        from: arg1 - 1,
                        to: arg2 as u8 - b'0' - 1,
                        amount: arg0,
                    })
                } else {
                    Some(Action::Move {
                        from: arg0 - 1,
                        to: arg1 - 1,
                        amount: 1,
                    })
                }
            }
            's' => Some(Action::StoreCard(input.next()? as u8 - b'0' - 1)),
            'p' => {
                let cell = input.next()? as u8 - b'0' - 1;
                let column = input.next()? as u8 - b'0' - 1;
                Some(Action::PlaceCard { cell, column })
            }
            'r' => Some(Action::StackCard(input.next()? as u8 - b'0' - 1)),
            'R' => Some(Action::StackCardFromCell(input.next()? as u8 - b'0' - 1)),
            'd' => Some(Action::CollectDragons(match input.next()? {
                'r' => Suit::Red,
                'b' => Suit::Black,
                'g' => Suit::Green,
                _ => return None,
            })),
            _ => None,
        }
    }

    pub fn solve(&self) -> Option<Vec<Action>> {
        let mut solution = None;
        self._solve(
            &mut self.clone(),
            &mut solution,
            &mut Vec::new(),
            &mut HashMap::new(),
        );
        solution
    }

    fn _solve(
        &self,
        current: &mut Self,
        current_solution: &mut Option<Vec<Action>>,
        taken_actions: &mut Vec<Action>,
        processed: &mut HashMap<String, usize>,
    ) -> bool {
        if let Some(current_solution) = current_solution {
            if current_solution.len() <= taken_actions.len() {
                return false;
            }
        }

        let hash = format!("{}", current);
        if processed
            .get(&hash)
            .is_some_and(|&l| l < taken_actions.len())
        {
            return false;
        }
        processed.insert(hash, taken_actions.len());

        if current.has_won() {
            return true;
        }

        let mut possible_actions = current.get_possible_actions();
        possible_actions.sort_unstable_by_key(|a| match a {
            Action::Move { from, to, amount } => {
                let from_col = &current.columns[*from as usize];
                let from = from_col.get(from_col.len() - *amount as usize);
                let above = from_col.get((from_col.len() - *amount as usize).wrapping_sub(1));
                let to = current.columns[*to as usize].last();
                match (from, to, above) {
                    (Some(from), _, Some(above)) => {
                        if from.can_place_on(Some(above)) {
                            15
                        } else {
                            1
                        }
                    }
                    (Some(_), Some(_), None) => 1,
                    (Some(_), None, None) => 15,
                    (None, _, _) => 15,
                }
            }
            Action::StoreCard(_) => 15,
            Action::PlaceCard { cell: _, column: _ } => 7,
            Action::StackCard(_) => 0,
            Action::StackCardFromCell(_) => 0,
            Action::CollectDragons(_) => 0,
        });

        // println!(
        //     "Possible actions: {}",
        //     possible_actions
        //         .iter()
        //         .map(|a| format!("{}", a))
        //         .collect::<Vec<_>>()
        //         .join(", ")
        // );
        for action in &possible_actions {
            // println!("{}", current);
            // println!("    V Doing: {} V", action);
            current.do_action(*action);
            taken_actions.push(*action);
            // println!("{}", current);
            if self._solve(current, current_solution, taken_actions, processed) {
                *current_solution = Some(taken_actions.clone());
                println!(
                    "{} steps: {}",
                    taken_actions.len(),
                    taken_actions
                        .iter()
                        .map(|a| format!("{}", a))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                taken_actions.pop();
                return false;
            }
            taken_actions.pop();
            *current = self.clone();
            for a in taken_actions.clone() {
                current.do_action(a);
            }
        }

        false
    }

    pub fn new() -> Self {
        Self::from_deck(Self::STANDARD_DECK)
    }

    pub fn from_deck(deck: [Card; 40]) -> Self {
        let columns: Vec<Vec<_>> = deck
            .chunks(Self::STANDARD_DECK.len() / Self::COLUMNS)
            .map(|column| column.to_vec())
            .collect();
        let card_cells = Vec::new();
        let card_stacks = vec![
            CardStack {
                suit: Suit::Red,
                value: 0,
            },
            CardStack {
                suit: Suit::Black,
                value: 0,
            },
            CardStack {
                suit: Suit::Green,
                value: 0,
            },
        ];
        Self {
            columns,
            card_cells,
            card_stacks,
        }
    }

    fn with_rng(rng: &mut impl Rng) -> Self {
        let mut deck = Self::STANDARD_DECK;
        deck.shuffle(rng);
        Self::from_deck(deck)
    }

    pub fn from_string(s: &str) -> Self {
        let mut columns = vec![Vec::new(); 8];
        let mut cards_strings = s.split(&[',', '\n', ' ']).filter(|s| !s.trim().is_empty());
        for _ in 0..5 {
            for col in columns.iter_mut() {
                let card_str = cards_strings
                    .next()
                    .expect("Not enough cards in input string");
                let card = Card::from_string(card_str)
                    .unwrap_or_else(|| panic!("Invalid card string: {}", card_str));
                col.push(card);
            }
        }

        Self {
            columns,
            ..Self::new()
        }
    }

    fn stringify(&self) -> String {
        let mut cards = Vec::with_capacity(Self::STANDARD_DECK.len());
        for row in 0..5 {
            for col in self.columns.iter() {
                if let Some(card) = col.get(row) {
                    cards.push(format!("{}", card));
                }
            }
        }
        cards.join(",")
    }
}

impl Default for Solitaire {
    fn default() -> Self {
        let mut rng = thread_rng();
        Self::with_rng(&mut rng)
    }
}

impl Display for Solitaire {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for cell in &self.card_cells {
            write!(f, "{} ", cell)?;
        }
        for _ in 0..(3 - self.card_cells.len()) {
            write!(f, "[] ")?;
        }

        write!(f, "{}", " ".repeat(6))?;

        for stack in &self.card_stacks {
            write!(f, "{} ", stack)?;
        }
        writeln!(f)?;
        for i in 1..8 {
            write!(f, " {}|", i)?;
        }
        write!(f, " 8")?;

        for row in 0.. {
            let mut is_row_empty = true;
            for column in &self.columns {
                if let Some(card) = column.get(row) {
                    if is_row_empty {
                        writeln!(f)?;
                    }
                    write!(f, "{} ", card)?;
                    is_row_empty = false;
                } else {
                    if row == 0 {
                        writeln!(f)?;
                    }
                    write!(f, "   ")?;
                };
            }

            if is_row_empty {
                break;
            }
        }

        Ok(())
    }
}

/// Play, generate, and solve Shenzhen Solitaire boards!
#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// Where to save the generated board
    #[arg(short, long)]
    outfile: Option<String>,

    /// File containing the board to use
    #[arg(short, long)]
    infile: Option<String>,

    /// Seed to use when generating the board
    #[arg(short, long)]
    seed: Option<u64>,

    /// Should the board be solved?
    #[arg(short, long, default_value_t = false)]
    auto: bool,
}

fn main() {
    let args = Args::parse();
    let mut solitaire = match (args.infile, args.seed) {
        (Some(inpath), _) => {
            let s = std::fs::read_to_string(&inpath)
                .unwrap_or_else(|e| panic!("Couldn't read from file {}: {}", inpath, e));
            Solitaire::from_string(&s)
        }
        (None, Some(seed)) => Solitaire::with_rng(&mut StdRng::seed_from_u64(seed)),
        (None, None) => Solitaire::default(),
    };

    if let Some(outpath) = args.outfile {
        let mut file = File::create(&outpath)
            .unwrap_or_else(|e| panic!("Failed to open outfile {}: {}", outpath, e));
        file.write_all(&[solitaire.stringify().as_bytes(), &[b'\n']].concat())
            .expect("Failed to write to the file");
    }
    solitaire.do_all_automatic_actions();

    if args.auto {
        if let Some(solution) = solitaire.solve() {
            println!(
                "Winning sequence: {}",
                solution
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        } else {
            println!("Unsolvable.");
        }
    }

    loop {
        let actions = solitaire.get_possible_actions();
        if actions.is_empty() {
            println!("{}", solitaire);
            println!("no possible moves. game lost!");
            break;
        }

        println!(
            "moves: {}",
            actions
                .iter()
                .filter_map(|a| match a {
                    Action::StoreCard(_) => None,
                    Action::Move {
                        from: _,
                        to,
                        amount: _,
                    } if solitaire.columns[*to as usize].is_empty() => None,
                    Action::PlaceCard { cell: _, column }
                        if solitaire.columns[*column as usize].is_empty() =>
                        None,
                    _ => Some(format!("{}", a)),
                })
                .collect::<Vec<_>>()
                .join(", ")
        );

        println!("{}", solitaire);

        let action = loop {
            let input = stdin().lock().lines().next().unwrap().unwrap();
            if let Some(action) = Solitaire::parse_input(&input) {
                if actions.contains(&action) {
                    break action;
                } else {
                    println!("Invalid action");
                }
            } else {
                println!("Invalid input");
            }
        };

        solitaire.do_action(action);

        if solitaire.has_won() {
            println!("{}", solitaire);
            println!("game won!");
            break;
        }
    }
}
