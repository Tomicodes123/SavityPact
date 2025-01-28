# SavityPact: Decentralized Savings Smart Contract

## Overview

SavityPact is a decentralized savings platform implemented as a smart contract on the Stacks blockchain. It enables users to create and manage personal and group savings goals with customizable unlock conditions. The platform also features a referral system to incentivize user growth.

## Features

- Personal savings goals
- Group savings goals
- Customizable unlock conditions
- Referral system with rewards
- Secure fund management

## Smart Contract Functions

### Goal Management

1. `create-personal-goal`: Create a new personal savings goal
2. `create-group-goal`: Create a new group savings goal
3. `deposit`: Add funds to a savings goal
4. `withdraw`: Withdraw funds from a goal (subject to conditions)

### Referral System

1. `register-with-referrer`: Register a new user with a referrer
2. `claim-referral-rewards`: Claim accumulated referral rewards
3. `set-referral-reward-percent`: Set the referral reward percentage (admin only)

### Read-Only Functions

1. `get-goal`: Retrieve information about a specific goal
2. `get-user-goals`: Get a list of a user's goals
3. `get-referrer`: Get the referrer of a user
4. `get-referral-rewards`: Check the referral rewards of a user

## Technical Details

- Language: Clarity
- Platform: Stacks blockchain
- Data structures: Maps for goals, user goals, referrals, and rewards

## Security Measures

- Input validation for all public functions
- Checks for unauthorized access and insufficient funds
- Prevention of self-referrals
- Limit on the number of goals per user

