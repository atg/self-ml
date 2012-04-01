require 'rubygems'
require 'require_relative'

module SelfML; end

['parser','ast','transformer'].each do |file|
  require_relative "selfml/#{file}"
end

module SelfML
  
  def self.parse(s)
    parser      = Parser.new
    transformer = Transformer.new
    
    tree = parser.parse(s)
    out  = transformer.apply(tree)
    
    out
  end
  
  def self.to_sml(s)
    s.map(&:serialize).join("\n\n") + "\n"
  end

end