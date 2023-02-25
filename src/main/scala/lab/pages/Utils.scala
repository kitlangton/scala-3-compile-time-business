package lab.pages

extension (str: String)
  def red: String        = Console.RED + str + Console.RESET
  def green: String      = Console.GREEN + str + Console.RESET
  def yellow: String     = Console.YELLOW + str + Console.RESET
  def blue: String       = Console.BLUE + str + Console.RESET
  def magenta: String    = Console.MAGENTA + str + Console.RESET
  def cyan: String       = Console.CYAN + str + Console.RESET
  def white: String      = Console.WHITE + str + Console.RESET
  def black: String      = Console.BLACK + str + Console.RESET
  def bold: String       = Console.BOLD + str + Console.RESET
  def dim: String        = "[2m" + str + Console.RESET
  def underlined: String = Console.UNDERLINED + str + Console.RESET
