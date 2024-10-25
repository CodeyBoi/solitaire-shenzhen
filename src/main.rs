use core::panic;
use std::{
    fmt::Display,
    io::{stdin, BufRead},
};

use rand::seq::SliceRandom;

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

            if let Some(&Card::Numbered { suit, value }) = column.last() {
                for stack in &self.card_stacks {
                    if suit == stack.suit && value == stack.value + 1 {
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

                if let &Card::Numbered { suit, value } = card {
                    for stack in &self.card_stacks {
                        if suit == stack.suit && value == stack.value + 1 {
                            actions.push(Action::StackCardFromCell(cell_idx as u8));
                        }
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

        loop {
            if !self.do_automatic_action() {
                break;
            }
        }
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
            panic!("tried to store card with no available card cells");
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
        let (suit, value) = match self.columns[column as usize].pop() {
            Some(Card::Numbered { suit, value }) => (suit, value),
            Some(Card::Dragon(_)) => panic!("tried to stack dragon"),
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
                "tried to stack {} on {}",
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
            + 2
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
                    action = Some(Action::StackCard(cell_idx as u8));
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
}

impl Default for Solitaire {
    fn default() -> Self {
        let mut rng = rand::thread_rng();
        let mut deck = Self::STANDARD_DECK;
        deck.shuffle(&mut rng);

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

        let mut solitaire = Self {
            columns,
            card_cells,
            card_stacks,
        };

        loop {
            if !solitaire.do_automatic_action() {
                break;
            }
        }

        solitaire
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
        writeln!(f, " 8")?;

        for row in 0.. {
            let mut is_done = true;
            for column in &self.columns {
                if let Some(card) = column.get(row) {
                    write!(f, "{} ", card)?;
                    is_done = false;
                } else {
                    write!(f, "   ")?;
                };
            }

            if is_done {
                break;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

fn main() {
    let mut solitaire = Solitaire::default();

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
