require 'rubygems'
require 'parslet'

class SelfML::Parser < Parslet::Parser
  # Helpers
  rule(:space)  { match('\s').repeat(1) }
  rule(:space?) { space.maybe }
  
  rule(:word)   { match('\w').repeat }
  rule(:word?)  { word.maybe }
  
  # String
  rule(:string)    { backtick.as(:backticks) | bracketed.as(:brackets) | verbatim.as(:verbatim) }
  rule(:backtick) do
    str('`') >> (
      str('``').as(:ticks) | match('``|[^`]').as(:text)
    ).repeat >> str('`')
  end
  rule(:bracketed) do
  	str('[') >> (
  	  bracketed.as(:nested) | match('[^\]]').as(:text)
    ).repeat >> str(']')
  end
  rule(:verbatim)  { str("#").absnt? >> match['^\[\](){}\s'].repeat(1) }

  # List
  rule(:tail) { (node >> space?).repeat }
  rule(:list) {
  	str('(') >> space? >> 
  	  comment.as(:comment).maybe >> space? >>
  	  string.as(:head)           >> space? >>
  	  tail.as(:tail)             >> space? >>
    space? >> str(')')
  }
  
  # Comment
  rule(:comment) { line.as(:line) | block.as(:block) }
  rule(:line)    { str('#') >> match['^\\n'].repeat.as(:text) >> (str("\n") | any.absnt?) }
  rule(:block) do
    str('{#') >> (
      block.as(:block) |  match['^#}'].as(:text)
    ).repeat >> str('#}')
  end

  # Root
  rule(:node) { comment.as(:comment) | list.as(:list) | string.as(:string)  }
  
  rule(:top) { (space? >> node >> space?).repeat }
  root(:top)
end