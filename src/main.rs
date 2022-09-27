use termion::{input::TermRead, raw::IntoRawMode};

#[derive(Debug, Clone, Copy, Eq)]
enum LevenshteinFrom {
	Start(usize),
	Top(usize),
	Left(usize),
	TopLeft(usize),
}

impl LevenshteinFrom {
	fn value(&self) -> usize {
		match self {
			LevenshteinFrom::Start(value) => *value,
			LevenshteinFrom::Top(value) => *value,
			LevenshteinFrom::Left(value) => *value,
			LevenshteinFrom::TopLeft(value) => *value,
		}
	}
}

impl PartialEq for LevenshteinFrom {
	fn eq(&self, other: &Self) -> bool {
		self.value() == other.value()
	}
}

impl PartialOrd for LevenshteinFrom {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.value().partial_cmp(&other.value())
	}
}

impl Ord for LevenshteinFrom {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let cmp = self.value().cmp(&other.value());
		if let std::cmp::Ordering::Equal = cmp {
			match (self, other) {
				(LevenshteinFrom::Start(_),   LevenshteinFrom::Start(_))   => std::cmp::Ordering::Equal,
				(LevenshteinFrom::Start(_),   _)                           => std::cmp::Ordering::Less,
				(_,                           LevenshteinFrom::Start(_))   => std::cmp::Ordering::Greater,
				(LevenshteinFrom::TopLeft(_), LevenshteinFrom::TopLeft(_)) => std::cmp::Ordering::Equal,
				(LevenshteinFrom::TopLeft(_), _)                           => std::cmp::Ordering::Less,
				(_,                           LevenshteinFrom::TopLeft(_)) => std::cmp::Ordering::Greater,
				(LevenshteinFrom::Top(_),     LevenshteinFrom::Top(_))     => std::cmp::Ordering::Equal,
				(LevenshteinFrom::Top(_),     _)                           => std::cmp::Ordering::Less,
				(_,                           LevenshteinFrom::Top(_))     => std::cmp::Ordering::Greater,
				(LevenshteinFrom::Left(_),    LevenshteinFrom::Left(_))    => std::cmp::Ordering::Equal,
			}
		}
		else {
			cmp
		}
	}
}

impl std::ops::Add<usize> for LevenshteinFrom {
	type Output = Self;

	fn add(self, rhs: usize) -> Self::Output {
		match self {
			LevenshteinFrom::Start(value) => LevenshteinFrom::Start(value + rhs),
			LevenshteinFrom::Top(value) => LevenshteinFrom::Top(value + rhs),
			LevenshteinFrom::Left(value) => LevenshteinFrom::Left(value + rhs),
			LevenshteinFrom::TopLeft(value) => LevenshteinFrom::TopLeft(value + rhs),
		}
	}
}

struct Levenshtein {
	input: String,
	target: String,
	matrix: Vec<Vec<LevenshteinFrom>>,
	min_final: (usize, usize), // value, index
	width: usize,
	height: usize,
	stop_col: usize,
}

impl std::fmt::Display for Levenshtein {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let width = f.precision();
		if let Some(width) = width {
			if width < 5 {
				return Err(std::fmt::Error);
			}
		}

		let bar_col = width.map(|width| (width + 1) / 2);

		let mut i = self.height - 1;
		let mut j = self.min_final.1;
		let mut v = vec![];
		while i != 0 || j != 0 {
			if let Some(bar_col) = bar_col {
				if v.len() == bar_col {
					break
				}
			}
			v.push((i, j, self.matrix[i][j]));
			match self.matrix[i][j] {
				LevenshteinFrom::TopLeft(_) => {
					i -= 1;
					j -= 1;
				},
				LevenshteinFrom::Top(_) => {
					i -= 1;
				},
				LevenshteinFrom::Left(_) => {
					j -= 1;
				},
				LevenshteinFrom::Start(_) => break,
			}
		}
		v.reverse();

		let mut lines = vec![String::new(); 4];
		let mut dists = vec![];
		for (i, j, value) in v.iter().copied() {
			let dist;
			if i != 0 && j != 0 {
				if let LevenshteinFrom::TopLeft(_) = value {
					dist = Self::dist(self.input.chars().nth(i - 1).unwrap(), self.target.chars().nth(j - 1).unwrap());
				}
				else {
					dist = 1;
				}
			}
			else {
				dist = 1;
			}
			dists.push(dist);
			// if dist != 0 {
			// 	write!(f, "{}", termion::color::Fg(termion::color::Red))?;
			// }
			match (value, j) {
				(_, 0) | (LevenshteinFrom::Top(_), _) => {
					lines[0].push(' ');
					lines[1].push(' ');
					// write!(f, " ")?;
					// write!(f, "{}{}", termion::cursor::Down(1), termion::cursor::Left(1))?;
					// write!(f, " ")?;
				},
				_ => {
					lines[0].push(self.target.chars().nth(j - 1).unwrap());
					lines[1].push('V');
					// write!(f, "{}", self.target.chars().nth(j - 1).unwrap())?;
					// write!(f, "{}{}", termion::cursor::Down(1), termion::cursor::Left(1))?;
					// write!(f, "V")?;
				}
			}
			// write!(f, "{}{}", termion::cursor::Down(1), termion::cursor::Left(1))?;
			match (value, i) {
				(_, 0) | (LevenshteinFrom::Left(_), _) => {
					lines[2].push(' ');
					lines[3].push(' ');
					// write!(f, " ")?;
					// write!(f, "{}{}", termion::cursor::Down(1), termion::cursor::Left(1))?;
					// write!(f, " ")?;
				},
				_ => {
					lines[2].push('^');
					lines[3].push(self.input.chars().nth(i - 1).unwrap());
					// write!(f, "^")?;
					// write!(f, "{}{}", termion::cursor::Down(1), termion::cursor::Left(1))?;
					// write!(f, "{}", self.input.chars().nth(i - 1).unwrap())?;
				}
			}
			// write!(f, "{}", termion::cursor::Up(3))?;
			// if dist != 0 {
			// 	write!(f, "{}", termion::color::Fg(termion::color::Reset))?;
			// }
		}
		if let Some(bar_col) = bar_col {
			if v.len() < bar_col {
				write!(f, "{}", termion::cursor::Right((bar_col - v.len()) as _))?;
			}
		}
		for line in lines.into_iter().map(|line| line.chars().zip(dists.iter().copied()).map(|(c, d)| if d == 0 { c.to_string() } else { format!("{}{}{}", termion::color::Fg(termion::color::Red), c, termion::color::Fg(termion::color::Reset)) }).collect::<String>()) {
			write!(f, "{}", termion::cursor::Save)?;
			write!(f, "{}|", line)?;
			writeln!(f, "{}", termion::cursor::Restore)?;
		}
		if let Some(bar_col) = bar_col {
			write!(f, "\r{}{}", termion::cursor::Up(1), termion::cursor::Right((bar_col + 1) as _))?;
			write!(f, "{}", termion::cursor::Save)?;
		}
		write!(f, "{}", termion::cursor::Up(3))?;

		dbg!(self.min_final.1 + width.unwrap() - bar_col.unwrap() - 1 - 1, self.width - 1);
		let s = &self.target[self.min_final.1..(width.map_or(self.width - 1, |w| (self.width - 1).min(self.min_final.1 + w - bar_col.unwrap_or(0) - 1 - 1)))];
		write!(f, "{}", s)?;
		write!(f, "{}", termion::cursor::Restore)?;

		Ok(())
	}
}

impl std::fmt::Debug for Levenshtein {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let thing_a: Vec<_> = self.input.chars().collect();
		write!(f, "  |    ")?;
		for char in self.target.chars() {
			write!(f, "{:2} ", char)?;
		}
		writeln!(f)?;
		write!(f, "--|--")?;
		for _ in self.target.chars() {
			write!(f, "---")?;
		}
		for (row_index, row) in self.matrix.iter().enumerate() {
			writeln!(f)?;
			if row_index == 0 {
				write!(f, "  |")?;
			}
			else {
				write!(f, "{:2}|", thing_a[row_index - 1])?;
			}
			for (col_index, cell) in row.iter().enumerate() {
				if col_index >= self.stop_col {
					write!(f, " - ")?;
				} else {
					if let LevenshteinFrom::Start(_) = cell {
						write!(f, "{}", termion::color::Fg(termion::color::Yellow))?;
					}
					if let LevenshteinFrom::Top(_) = cell {
						write!(f, "{}", termion::color::Fg(termion::color::Red))?;
					}
					if let LevenshteinFrom::Left(_) = cell {
						write!(f, "{}", termion::color::Fg(termion::color::Green))?;
					}
					if let LevenshteinFrom::TopLeft(_) = cell {
						write!(f, "{}", termion::color::Fg(termion::color::Blue))?;
					}
					if col_index == self.min_final.1 && row_index == self.height - 1 {
						write!(f, "{}", termion::color::Fg(termion::color::Cyan))?;
					}
					write!(f, "{:2} ", cell.value())?;
				}
				write!(f, "{}", termion::color::Fg(termion::color::Reset))?;
				if col_index >= self.stop_col {
					break;
				}
			}
		}
		Ok(())
	}
}

impl Levenshtein {
	pub fn new(input_text: &str, target_text: &str) -> Self {
		let height = input_text.chars().count() + 1;
		let width = target_text.chars().count() + 1;

		let mut levenshtein = Self {
			input: input_text.to_string(),
			target: target_text.to_string(),
			matrix: vec![vec![LevenshteinFrom::Start(usize::MAX); width]; height],
			min_final: (usize::MAX, 0),
			width,
			height,
			stop_col: width,
		};

		for j in 0..width {
			let mut all_bigger = true;
			for i in 0..height {
				if i == 0 && j == 0 {
					levenshtein.matrix[i][j] = LevenshteinFrom::Start(0);
				}
				else if i == 0 {
					levenshtein.matrix[i][j] = LevenshteinFrom::Left(levenshtein.matrix[i][j - 1].value() + 1);
				}
				else if j == 0 {
					levenshtein.matrix[i][j] = LevenshteinFrom::Top(levenshtein.matrix[i - 1][j].value() + 1);
				}
				else {
					let min = levenshtein.min_around(i, j);
					let mut diff = Self::dist(target_text.chars().nth(j - 1).unwrap(), input_text.chars().nth(i - 1).unwrap());
					if let LevenshteinFrom::Left(_) = min {
						if j > 1 && diff == 0 && Self::dist(target_text.chars().nth(j - 2).unwrap(), input_text.chars().nth(i - 1).unwrap()) == 0 {
							diff = 1;
						}
					}
					levenshtein.matrix[i][j] = min + diff;
				}
				if levenshtein.matrix[i][j].value() <= levenshtein.min_final.0 {
					all_bigger = false;
				}
			}
			if j == 0 || levenshtein.matrix[height - 1][j].value() <= levenshtein.min_final.0  {
				levenshtein.min_final = (levenshtein.matrix[height - 1][j].value(), j);
			}
			else if all_bigger {
				levenshtein.stop_col = j;
				break ;
			}
		}

		levenshtein
	}

	pub fn add_char(&mut self, c: char) {
		self.input.push(c);
		self.height += 1;
		self.matrix.push(vec![LevenshteinFrom::Start(usize::MAX); self.width]);
		self.matrix[self.height - 1][0] = LevenshteinFrom::Top(self.height - 1);
		self.min_final = (self.height - 1, 0);
		for j in 1..self.stop_col {
			let min = self.min_around(self.height - 1, j);
			let mut diff = Self::dist(self.target.chars().nth(j - 1).unwrap(), c);
			if let LevenshteinFrom::Left(_) = min {
				if j > 1 && diff == 0 && Self::dist(self.target.chars().nth(j - 2).unwrap(), c) == 0 {
					diff = 1;
				}
			}
			self.matrix[self.height - 1][j] = min + diff;
			if self.matrix[self.height - 1][j].value() <= self.min_final.0 {
				self.min_final = (self.matrix[self.height - 1][j].value(), j);
			}
		}
		for j in self.stop_col..self.width {
			let mut all_bigger = true;
			for i in 0..self.height {
				if i == 0 && j == 0 {
					self.matrix[i][j] = LevenshteinFrom::Start(0);
				}
				else if i == 0 {
					self.matrix[i][j] = LevenshteinFrom::Left(self.matrix[i][j - 1].value() + 1);
				}
				else if j == 0 {
					self.matrix[i][j] = LevenshteinFrom::Top(self.matrix[i - 1][j].value() + 1);
				}
				else {
					let min = self.min_around(i, j);
					let mut diff = Self::dist(self.target.chars().nth(j - 1).unwrap(), self.input.chars().nth(i - 1).unwrap());
					if let LevenshteinFrom::Left(_) = min {
						if j > 1 && diff == 0 && Self::dist(self.target.chars().nth(j - 2).unwrap(), self.input.chars().nth(i - 1).unwrap()) == 0 {
							diff = 1;
						}
					}
					self.matrix[i][j] = min + diff;
				}
				if self.matrix[i][j].value() <= self.min_final.0 {
					all_bigger = false;
				}
			}
			if self.matrix[self.height - 1][j].value() <= self.min_final.0  {
				self.min_final = (self.matrix[self.height - 1][j].value(), j);
			}
			else if all_bigger {
				self.stop_col = j;
				break ;
			}
			if j == self.width - 1 {
				self.stop_col = self.width;
			}
		}
	}

	pub fn remove_char(&mut self) {
		if self.height <= 1 {
			return ;
		}
		self.input.pop();
		self.height -= 1;
		self.matrix.pop();
		self.min_final = (self.height - 1, 0);
		for j in 1..self.stop_col {
			if self.matrix[self.height - 1][j].value() <= self.min_final.0 {
				self.min_final = (self.matrix[self.height - 1][j].value(), j);
			}
		}
	}

	fn dist(a: char, b: char) -> usize {
		let mut dist = 0;
		if a.is_uppercase() != b.is_uppercase() && a.to_lowercase().to_string() != a.to_uppercase().to_string() && b.to_lowercase().to_string() != b.to_uppercase().to_string() {
			dist += 1;
		}
		if a.to_lowercase().to_string() != b.to_lowercase().to_string() {
			dist += 1;
		}
		dist
	}

	fn min_around(&self, i: usize, j: usize) -> LevenshteinFrom {
		let mut around = vec![];
		if i > 0 {
			around.push(LevenshteinFrom::Top(self.matrix[i - 1][j].value()));
			if j > 0 {
				around.push(LevenshteinFrom::TopLeft(self.matrix[i - 1][j - 1].value()));
			}
		}
		if j > 0 {
			around.push(LevenshteinFrom::Left(self.matrix[i][j - 1].value()));
		}
		around.iter().cloned().min().unwrap_or(LevenshteinFrom::Start(0))
	}
}

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

fn main() -> Result<()> {
	use std::io::Write;
	// let target_text = "HeLlo, world!a";
	// let input_text = "lL Weourl";
	// let input_text = "say first then nation much person long largeway from have willinteerst because great than over lead not also end off aign point mean end pbilc most old even under consider want help";
	let input_text = "";
	let target_text = "say first then nation much person long large way from have will interest because great than over lead not also end off again point mean end public most old even under consider want help thing who part";

	let mut levenshtein = Levenshtein::new(input_text, target_text);

	// levenshtein.add_char('l');

	// println!("{:?}", levenshtein);
	// return Ok(());

	let mut screen = termion::screen::AlternateScreen::from(std::io::stdout()).into_raw_mode()?;
	// let mut screen = std::io::stdout().into_raw_mode()?;
	write!(screen, "{}", termion::clear::All)?;
	write!(screen, "{}", termion::cursor::Goto(1, 1))?;
	let (width, height) = termion::terminal_size()?;
	write!(screen, "{:#.*}", &(width as usize), levenshtein)?;
	screen.flush()?;
	let stdin = std::io::stdin();
	for k in stdin.keys() {
		let k = k?;
		match k {
			termion::event::Key::Ctrl('c') | termion::event::Key::Esc => {
				break ;
			},
			termion::event::Key::Char('\t') => {
				for _ in 1..levenshtein.height {
					levenshtein.remove_char();
				}
				write!(screen, "{}", termion::clear::All)?;
				write!(screen, "{}", termion::cursor::Goto(1, 1))?;
				let (width, height) = termion::terminal_size()?;
				write!(screen, "{:#.*}", &(width as usize), levenshtein)?;
				screen.flush()?;
			},
			termion::event::Key::Ctrl('w') => {
				while levenshtein.input.len() > 0 && levenshtein.input.chars().last().unwrap().is_whitespace() {
					levenshtein.remove_char();
				}
				let chars = levenshtein.input.split_whitespace().last().map(|w| w.len()).unwrap_or(0);
				for _ in 0..chars {
					levenshtein.remove_char();
				}
				write!(screen, "{}", termion::clear::All)?;
				write!(screen, "{}", termion::cursor::Goto(1, 1))?;
				let (width, height) = termion::terminal_size()?;
				write!(screen, "{:#.*}", &(width as usize), levenshtein)?;
				screen.flush()?;
			},
			termion::event::Key::Char(c) => {
				levenshtein.add_char(c);
				write!(screen, "{}", termion::clear::All)?;
				write!(screen, "{}", termion::cursor::Goto(1, 1))?;
				let (width, height) = termion::terminal_size()?;
				write!(screen, "{:#.*}", &(width as usize), levenshtein)?;
				screen.flush()?;
			},
			termion::event::Key::Backspace => {
				levenshtein.remove_char();
				write!(screen, "{}", termion::clear::All)?;
				write!(screen, "{}", termion::cursor::Goto(1, 1))?;
				let (width, height) = termion::terminal_size()?;
				write!(screen, "{:#.*}", &(width as usize), levenshtein)?;
				screen.flush()?;
			},
			_ => {},
		}
	}
	Ok(())
}

/*






*/
