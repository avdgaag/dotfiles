# print SQL to STDOUT
if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end

# Autocompletion
require 'irb/completion'

IRB.conf[:PROMPT_MODE] = :SIMPLE
# Prompt behaviour
ARGV.concat ['--readline', '--prompt-mode', 'simple']

# History
require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

# List object's local methods
class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end

def y(*args)
  args.each { |arg| puts arg.to_yaml }
  nil
end

def copy(*args)
  IO.popen('pbcopy', 'r+') do |clipboard|
    clipboard.puts args.map(&:inspect)
  end
end
