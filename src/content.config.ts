import { defineCollection, z } from 'astro:content';

const science = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    description: z.string(),
    date: z.coerce.date(),
    category: z.string(),
  }),
});

export const collections = {
  science,
};