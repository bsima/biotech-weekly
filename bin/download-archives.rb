#!/usr/bin/env ruby

require 'httparty'
require 'nokogiri'

class BiotechWeeklyArchive
  include HTTParty
  base_uri 'www.biotechweekly.com'

  def archive
    self.class.get("/archive")
  end

  def hrefs
    # Returns a list of the hrefs from the archive page
    parsed = Nokogiri::HTML(archive)
    parsed.xpath('//*[@id="public-issue-reader"]/div/ul/li/a/@href').map do |a|
      a.value
    end
  end

  def newsletter(slug)
    # Downloads and parses the newsletter
    page = self.class.get("/" + slug)
    parsed = Nokogiri::HTML(page).css('.landing-site-email-content')
  end

  def frontmatter(date, issue)
    # Generates YAML frontmatter for an issue
   %{- - -
title: Biotech Weekly ##{issue}
date: #{date}
issue: #{issue}
- - -

}
  end

  def filename(slug)
    # replace slashes with dashes in slug
    slug.gsub(/(?<!^)\//, "-") + ".html"
  end

  def download_all_emails
    # Does the thing.
    hrefs.map do |slug|
      name = filename(slug)
      date = slug.scan(/\d{4}\/\d{2}\/\d{2}/)[0]
      issue = slug.scan(/\d+$/)[0]
      head = frontmatter(date, issue)
      content = newsletter(slug)

      if issue < 60 && issue != nil
        puts "writing  : source/archive" + name

        File.open("source/archive" + name, "a+") { |f|
          f << head
          f << content
        }
      else
        puts "skipping : source/archive" + name
      end
    end
  end
end

bw = BiotechWeeklyArchive.new()
bw.download_all_emails()
