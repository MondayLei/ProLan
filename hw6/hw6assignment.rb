# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  My_All_Pieces = [rotations([[-1, 1], [-1, 0], [0, 1], [0, 0], [1, 0]]),  # 2*2 square plus one
                  [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],  # 1*5 
                  [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]], 
                  rotations([[0, 1], [0, 0], [-1, 0]])] + Piece::All_Pieces  # small L

  # Cheat piece
  Cheat_Piece = [[[0, 0]]]
  
  # your enhancements here
  def self.next_cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end

  def self.next_piece (board)
    MyPiece.new(My_All_Pieces.sample, board)
  end
end
	
class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
		@is_cheat = false		# Cheat Model
  end

  def next_piece
		if @is_cheat
			@current_block = MyPiece.next_cheat_piece(self)
			@is_cheat = false
		else
			@current_block = MyPiece.next_piece(self)
		end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.size.times{|index| 			# {0..locations.size-1}
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
	
	def rotate_180_degree
   if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
	end
	
	def cheat
		if @score >= 100 && @is_cheat == false
			@score -= 100
			@is_cheat = true
		end
	end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
	
	def key_bindings
		super
		@root.bind('u', lambda {@board.rotate_180_degree})
		
		@root.bind('c', lambda {@board.cheat})
	end
end
	
class MyPieceChallenge < MyPiece
  # color array
	My_All_Colors = ['#FFFF00', '#C0FF3E', '#B0E2FF', '#ADFF2F', '#7FFF00', '#54FF9F', '#4876FF	', '#3A5FCD', '#00FFFF', '#0000EE']
	
	def self.next_cheat_piece(board)
    MyPieceChallenge.new(Cheat_Piece, board)
  end

  def self.next_piece (board)
    MyPieceChallenge.new(My_All_Pieces.sample, board)
  end
	
	# Create a new Piece
	def initialize (point_array, board)
    @all_rotations = point_array
    @rotation_index = (0..(@all_rotations.size-1)).to_a.sample
		#print My_All_Pieces.index(@all_rotations)
    @color = My_All_Colors[My_All_Pieces.index(@all_rotations)]  # My_All_Colors.sample
    @base_position = [5, 0] # [column, row]
    @board = board
    @moved = true
  end
end
	
class MyBoardChallenge < MyBoard
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPieceChallenge.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
		@is_cheat = false		# Cheat Model
  end
	
	def next_piece
		if @is_cheat
			@current_block = MyPieceChallenge.next_cheat_piece(self)
			@is_cheat = false
		else
			@current_block = MyPieceChallenge.next_piece(self)
		end
    @current_pos = nil
  end

	def quicker
    if !game_over? and @game.is_running?
      @current_block.move(0, 1, 0)
    end
    draw
	end
	
	def changescore
		@score+=500
	end
end

class MyTetrisChallenge < MyTetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
	
	def key_bindings
		super		
		@root.bind('k', lambda {@board.quicker})
		@root.bind('l', lambda {@board.changescore})
	end
	
end
