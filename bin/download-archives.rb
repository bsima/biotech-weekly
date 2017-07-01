#!/usr/bin/env ruby
#
# A quick script to download all the articles from the old Goodbits
# website. To use, simply do:
#
#     bin/download-archives.rb


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

  def frontmatter(date, issue)
    # Generates YAML frontmatter for an issue
   %{---
title: Biotech Weekly ##{issue}
date: #{date}
issue: #{issue}
---

}
  end

  def fetch_newsletter!(slug)
    # Downloads and parses the newsletter
    page = self.class.get("/" + slug)
    parsed = Nokogiri::HTML(page).css('.landing-site-email-content')
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

      if issue != nil && issue.to_i > 59
        puts "writing: content/archive" + name

        content = fetch_newsletter!(slug)

        File.open("content/archive" + name, "a+") { |f|
          f << head
          f << content
        }
      end
    end
  end
end

main = BiotechWeeklyArchive.new()
main.download_all_emails()
