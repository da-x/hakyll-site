---
excerpt: Mail
title: Working with mailing lists under GMail
published: 2016-03-15
author: dan
---

**TL;DR: In the Webmail era, E-Mail for group correspondences is often mishandled. Here's how to efficiently handle it in GMail. **

With many open source projects, you may innocuously subscribe to an Mailing list on the Internet, or you may be unwittingly added to a group mail in an organization (those are often the `all@` and `dev@` and `rnd@` E-Mail prefixes). Your in-box is now bombed with messages that are not for you, in discussions you don't (yet) participate, but some of them are or would be, later.

At that point, you should be made aware of the situation and use GMail filters because they are easy to configure. However, the filters you choose play a crucial part on how the mailing list is handled. What so are the options?

__The bad options__

* **Bad** option #1: Skip in-box for everything sent to the mailing list. Now your GMail archive is some unsorted junk.
* **Bad** option #2: Delete everything sent to the mailing list that is not for me. Now, you cannot interject on discussions that could be relevant to your work.
* **Bad** option #3: Tag all posts to the mailing list with a label, and skip the in-box. We are getting there - however it's still bad. Because messages to you are archived as well (i.e. disappeared from in-box but still searchable, in GMail's terminology).

__One good option__

What we should expect from GMail is the following:

* If a group E-Mail is received, then:
    + If your personal E-Mail address is _not also_ in the `To:` or `Cc:` fields, then *skip in-box* and *apply a label*. The GMail will remain is unread for later consumption.
    + Otherwise, _only apply the label_ but _don't skip the in-box_. Since you are personally addressed in addition to the group, the discussion is relevant and your response is needed.

The nice thing about Labels in GMail is that they map nicely to groups under this scheme. When you visit the label, you are essentially visiting the group, and you can join discussions. One of nice benefits is that GMail tracks which discussions were read or unread by you, without cluttering your inbox.

__Adding GMail filters for Mailing List that Just Do The Proper Job:__

When viewing details for an E-Mail, GMail shows a link 'Filter messages from this mailing list'. It presents you with a bad filter that you can edit. However I think it's better to add the good set of filters from the outset instead of editing bad ones.

Go to one of the group E-Mails, and do *Show Original*, then look for the `List-Id` header. It would most likely appear similarly to this:

```
List-ID: <group.company.com>
```

To go along the rules of the option above, we should have filters that look like below, in the GMail's filter settings:

<img src="/images/gmail-filter-settings.png" class="center">

We can manually enter these filters in a few steps.

* The first GMail filter is (after relevant **strings replaced**):

    `list:group.company.com to:your.name@organization.com`

**How to enable:** Enter in GMail search box, hit tiny arrow in the right of the search box -> *Show Search Options* -> *Create As Filter* -> Do this: Apply label "Group Name"


* The second GMail filter is (after relevant **strings replaced**):

    `list:group.company.com -to:your.name@organization.com`

**How to enable:** Enter in GMail search box, hit tiny arrow in the right of the search box -> *Show Search Options* -> *Create As Filter* -> Do this: Skip In-box, Apply label "Group Name"

Alternatively, instead of the manual filter entry, it's possible to edit the following XML, and import it under GMail's filter settings.

```
<?xml version='1.0' encoding='UTF-8'?><feed xmlns='http://www.w3.org/2005/Atom' xmlns:apps='http://schemas.google.com/apps/2006'>
	<title>Mail Filters</title>
	<id>tag:mail.google.com,2008:filters:1458024047214,1458024071178</id>
	<entry>
		<category term='filter'></category>
		<title>Mail Filter</title>
		<id>tag:mail.google.com,2008:filter:1458024047214</id>
		<apps:property name='hasTheWord' value='list:group.company.com to:your.name@organization.com'/>
		<apps:property name='label' value='Dev'/>
		<apps:property name='sizeOperator' value='s_sl'/>
		<apps:property name='sizeUnit' value='s_smb'/>
	</entry>
	<entry>
		<category term='filter'></category>
		<title>Mail Filter</title>
		<id>tag:mail.google.com,2008:filter:1458024071178</id>
		<apps:property name='hasTheWord' value='list:group.company.com -to:your.name@organization.com'/>
		<apps:property name='label' value='Dev'/>
		<apps:property name='shouldArchive' value='true'/>
		<apps:property name='sizeOperator' value='s_sl'/>
		<apps:property name='sizeUnit' value='s_smb'/>
	</entry>
</feed>
```
