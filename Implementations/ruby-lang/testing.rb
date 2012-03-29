require 'rubygems'

require 'require_relative'
require 'awesome_print'
require 'differ'

require_relative 'lib/selfml.rb'

module SelfML
  
  def self.parse(s)
    parser      = Parser.new
    transformer = Transformer.new

    tree = parser.parse(s)
    out  = transformer.apply(tree)

    out
  end
  
  def self.to_sml(s)
    s.map(&:serialize).reduce("") {|sum,n| "#{sum}\n\n#{n}" }.strip
  end

end

### Testing Facilities
def load_tests(name)
  [File.read("self-ml/Testing/Testcases/#{name}.selfml") , File.read("self-ml/Testing/Outputs/output.#{name}.selfml").strip]
end

basic_in    , basic_out    = load_tests("basic")
comments_in , comments_out = load_tests("comments")
strings_in  , strings_out  = load_tests("strings")

puts SelfML.to_sml( SelfML.parse(basic_in)    ) == basic_out   #done
puts SelfML.to_sml( SelfML.parse(strings_in)  ) == strings_out #done
puts SelfML.to_sml( SelfML.parse(comments_in) ) == comments_out
puts Differ.diff_by_line( SelfML.to_sml( SelfML.parse(comments_in) ), comments_out)