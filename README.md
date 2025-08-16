# 🤝 Taskfund - Decentralized Freelance Escrow

A trustless smart contract platform for managing freelance work with milestone-based payments.

## 🎯 Features

- Create tasks with multiple milestones
- Secure escrow of funds
- Milestone-based payment release
- DAO fee system
- Automated payment distribution

## 💡 How It Works

1. **Creating a Task**
   - Client creates a task specifying:
     - Freelancer address
     - Total payment amount
     - Number of milestones
   - Funds are locked in the contract

2. **Managing Milestones**
   - Client adds milestone descriptions
   - Freelancer marks milestones as complete
   - Client approves and releases payment
   - DAO receives small fee from each payment

## 🔧 Usage

### Create a New Task
```clarity
(contract-call? .taskfund create-task 
    'FREELANCER_ADDRESS 
    u1000000 
    u4)
```

### Add Milestone
```clarity
(contract-call? .taskfund add-milestone 
    u1 
    u1 
    "Complete frontend design")
```

### Complete Milestone
```clarity
(contract-call? .taskfund complete-milestone 
    u1 
    u1)
```

### Approve and Release Payment
```clarity
(contract-call? .taskfund approve-milestone 
    u1 
    u1)
```

## 📊 Contract Details

- Default DAO fee: 5% (50 basis points)
- All amounts in microSTX
- Milestone payments are distributed equally



